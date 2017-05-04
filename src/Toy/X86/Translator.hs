{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE ViewPatterns    #-}

module Toy.X86.Translator
    ( compile
    , produceBinary
    ) where

import           Control.Lens          (Lens', at, ix, (%~), (&), (+=), (-=), (<&>), (^.),
                                        (^?))
import           Control.Monad         (forM, join)
import           Control.Monad.State   (get, runState)
import           Control.Monad.Trans   (MonadIO (..))
import           Data.Functor          (($>))
import qualified Data.Map              as M
import           Data.Maybe            (fromMaybe)
import           Data.Monoid           ((<>))
import qualified Data.Set              as S
import           Data.Text             (Text)
import qualified Data.Vector           as V
import           Formatting            (formatToString, shown, (%))
import qualified Formatting            as F
import           GHC.Exts              (fromList)
import           System.FilePath.Posix ((</>))
import           System.Process        (proc)

import           Toy.Exp               (FunSign (..), Var)
import qualified Toy.SM                as SM
import           Toy.X86.Data          (Inst (..), Insts, Operand (..), Program (..),
                                        StackDirection (..), eax, edx, jmp, ret,
                                        traverseOperands, (?))
import           Toy.X86.Optimize      (optimize)
import           Toy.X86.SymStack      (SymStackHolder, SymStackSpace, allocSymStackOp,
                                        popSymStackOp, runSymStackHolder, symStack,
                                        symStackSize, wipeSymStackAfterRollout)
import           Toy.X86.Util          (readCreateProcess)

compile :: SM.Insts -> Insts
compile = mconcat . map compileFun . separateFuns

compileFun :: SM.Insts -> Insts
compileFun insts =
    let (name, args) = case insts ^? ix 0 of
            Just (SM.Enter n argsInfo) -> (n, reverse argsInfo)  -- TODO: ???
            _                          -> error "Where is my Enter?!"
        vars    = foldr S.delete (foldMap gatherLocals insts) args
        locSP   = fromIntegral symStSpace
        ilocals = M.fromList $ zip (S.toList vars ++ ["<ra>"] ++ args) (from locSP)
        -- TODO: this won't work
        -- if symStSpace > length ilocals, we have to numerate arguments from
        -- another position, not zero
        -- moreover, this entire model doesn't allow to put our arguments to
        -- function we call, because while we push arguments on stack
        -- relative position of arguments changes
        (symStSpace, body) = fmap (join . fmap fromList)
                           $ runSymStackHolder $ mapM (step name ilocals) insts
        post    = [ resolveStackRefs symStSpace
                  , fixMemRefs
                  , insertExit
                  , mkStackShift $ length vars + fromIntegral symStSpace
                  , optimize
                  ] :: [Insts -> Insts]
    in foldl (&) body post
  where
    -- 'enumFrom' isn't lazy on it's argument, so using it
    from k = k : from (k + 1)

-- TODO: correct errors processing
step :: Var -> M.Map Var Int -> SM.Inst -> SymStackHolder [Inst]
step calleeName locals = \case
    SM.Nop        -> pure []
    SM.Push v     -> allocSymStackOp <&> \op -> [Mov (Const v) op]
    SM.Drop       -> popSymStackOp $> []
    SM.Load v     ->
        case locals ^. at v of
            Nothing  -> error "No such variable"
            Just idx -> allocSymStackOp <&> \op -> [Mov (Mem idx) eax, Mov eax op]
    SM.Store v    ->
        case locals ^. at v of
            Nothing  -> error "undetected variable!"
            Just idx -> popSymStackOp <&> \op -> [Mov op eax, Mov eax (Mem idx)]
    SM.Read      -> step calleeName locals $ SM.Call (FunSign "read" undefined)
    SM.Write     -> step calleeName locals $ SM.Call (FunSign "write" undefined)
    SM.Bin op    -> do
        op2 <- popSymStackOp
        op1 <- popSymStackOp
        resOp <- allocSymStackOp
        return $ binop op1 op2 op <> [Mov op2 eax, Mov eax resOp]
    SM.Label lid -> pure [Label lid]
    SM.Jmp   lid -> pure [jmp (SM.CLabel lid)]
    SM.JmpIf lid -> popSymStackOp <&> \op -> [Mov op eax, BinOp "cmp" (Const 0) eax, Jmp "ne" (SM.CLabel lid)]
    SM.Call (FunSign name _) -> do
        part1 <- rolloutSymStackOps [Call name]
        part2 <- allocSymStackOp <&> \op -> [Mov eax op]
        return $ part1 <> part2
    SM.Ret       -> do
        p1 <- popSymStackOp <&> \op -> [Mov op eax]
        return $ p1 <> [jmp (SM.ELabel calleeName)]
    SM.Enter{}   -> pure [NoopOperator "int3"]

separateFuns :: SM.Insts -> [SM.Insts]
separateFuns insts =
    case V.findIndex (\(i, v) -> isEnter v && i > 0) $ V.indexed insts of
        Nothing -> [insts]
        Just k  -> let (funInsts, remaining) = V.splitAt k insts
                   in  funInsts : separateFuns remaining
  where
    isEnter SM.Enter{} = True
    isEnter _          = False

gatherLocals :: SM.Inst -> S.Set Var
gatherLocals (SM.Store v) = [v]
gatherLocals _            = []

funBodyInsts :: Lens' Insts Insts
funBodyInsts f insts =
    let funLabelPos = fromMaybe (error "No fun label!")
                    $ V.findIndex isFunLabel insts
        (prelude, postlude) = V.splitAt (funLabelPos + 1) insts
        (body   , end)      = cutLast postlude
    in  if V.last insts /= ret then noRetAtEnd
        else f body <&> \body' -> prelude <> body' <> end
  where
    cutLast v = V.splitAt (V.length v - 1) v

    isFunLabel (Label (SM.FLabel _)) = True
    isFunLabel _                     = False

    noRetAtEnd = -- TODO: simplify
        error $ formatToString ("No 'Ret' at the end of function:\n"%shown)
        insts

insertExit :: Insts -> Insts
insertExit = (<> [ret])

mkStackShift :: Int -> Insts -> Insts
mkStackShift shift =
    funBodyInsts %~ \funBody -> mconcat
        [ [ResizeStack Forward shift]
        , funBody
        , [ResizeStack Backward shift]
        ]

resolveStackRefs :: Traversable f => SymStackSpace -> f Inst -> f Inst
resolveStackRefs (fromIntegral -> s) = fmap $ traverseOperands %~ resolve
  where
    resolve (Mem a  ) = Mem (a + s)
    resolve (Stack a) = Mem a
    resolve other     = other

-- | Function 'step', when sets `Mem` indices, doesn't take into account
-- possible @Push@es and @Pop@s to stack. This functions fixes it.
--
-- This is done as post-processing because, unlike SM, in X86 (real) stack is
-- influenced by not many operations.
fixMemRefs :: Traversable f => f Inst -> f Inst
fixMemRefs insts =
    let (res, finalStackShift) = flip runState 0 $ forM insts $ \case
            op@(Push _)    -> (id += 1) $> op
            op@(ResizeStack Forward k)
                           -> (id += k) $> op
            op@(ResizeStack Backward k)
                           -> (id -= k) $> op
            (Mov o1 o2)    -> Mov <$> fixMemRef o1 <*> fixMemRef o2
            op             -> return op
    in  case finalStackShift of
        0     -> res
        extra -> error $ "Detected extra values at stack at the end: "
                       ++ show extra

  where
    fixMemRef (Mem i) = Mem . (i +) <$> get
    fixMemRef other   = return other

-- | Puts symbolic stack on real stack. Symbolic stack becomes empty
rolloutSymStackOps :: [Inst] -> SymStackHolder [Inst]
rolloutSymStackOps insts = do
    -- when symstack uses registers only, we can just add some space on stack
    -- and put values there
    -- othersize, we are putting all registers on stack, remaining part of
    -- symstack will already lie where necessary
    toRollOut <- min (length symStack) <$> symStackSize
    let rolling = reverse (take toRollOut $ V.toList symStack) <&> Push
    wipeSymStackAfterRollout
    return $ mconcat
        [ rolling
        , insts
        , [ResizeStack Backward toRollOut]
        ]

inRegsWith
    :: Operand              -- operand to temporally store argument in
    -> Operand              -- argument
    -> (Operand -> [Inst])  -- action with given argument, placed in register
    -> [Inst]
inRegsWith _   op@(Reg _)   f = f op
inRegsWith aux op@(Const _) f = [Mov op aux] <> f aux
inRegsWith aux op           f = [Mov op aux] <> f aux <> [Mov aux op]

inRegs1 :: Operand -> (Operand -> [Inst]) -> [Inst]
inRegs1 = inRegsWith eax

-- inRegs2 :: Operand -> Operand -> (Operand -> Operand -> [Inst]) -> [Inst]
-- inRegs2 op1 op2 f = inRegsWith eax op1 $ inRegsWith edx op2 . f

binop :: Operand -> Operand -> Text -> [Inst]
binop op1 op2 = \case
    "+" -> inRegs1 op1 $ \op1' -> [BinOp "addl" op1' op2]
    "-" -> inRegs1 op1 $ \op1' -> [BinOp "subl" op2 op1', Mov op1' op2]  -- TODO: ???
    "*" -> inRegs1 op2 $ \op2' -> [BinOp "imull" op1 op2']
    "/" -> idiv eax
    "%" -> idiv edx

    "<"  -> cmp "g"  -- TODO: ?????
    ">"  -> cmp "l"
    "<=" -> cmp "ge"
    ">=" -> cmp "le"
    "==" -> cmp "e"
    "!=" -> cmp "ne"

    "^" -> inRegs1 op1 $ \op1' -> [BinOp "xorl" op1' op2]
    "&" -> inRegs1 op1 $ \op1' -> [BinOp "andl" op1' op2]
    "|" -> inRegs1 op1 $ \op1' -> [BinOp "orl" op1' op2]
    "&&" -> "&&" ?
        [ BinOp "xor" eax eax
        , Mov op1 edx
        , BinOp "andl" edx edx
        , UnaryOp "setne" (Reg "ah")
        , Mov op2 edx
        , BinOp "andl" edx edx
        , UnaryOp "setne" (Reg "al")
        , BinOp "and" (Reg "ah") (Reg "al")
        , BinOp "xor" (Reg "ah") (Reg "ah")
        , Mov eax op2
        ]
    "||" -> inRegsWith edx op2 $ \op2' -> "||" ?
        [ BinOp "xor" eax eax
        , BinOp "orl" op1 op2'
        , UnaryOp "setne" (Reg "al")
        , Mov eax op2'
        ]

    unknown -> error $ "Unsupported operation: " ++ show unknown
  where
    idiv res = "/" ?
        [ Mov op1 eax
        , NoopOperator "cdq"
        , UnaryOp "idivl" op2
        , Mov res op2
        ]
    cmp kind = inRegsWith edx op2 $ \op2' -> "cmp" ?
        [ BinOp "xor" eax eax
        , BinOp "cmp" op1 op2'
        , Set kind (Reg "al")
        , Mov eax op2'
        ]

produceBinary
    :: MonadIO m
    => FilePath
    -> FilePath
    -> Insts
    -> m (Either String ())
produceBinary runtimePath outputPath insts = liftIO $ do
    let cmd = proc "gcc"
            ["-m32"                        -- for x32
            , runtimePath </> "runtime.o"
            , "-xassembler"                -- specifies language
            , "-"                          -- take source from stdin
            , "-o", outputPath
            ]
    readCreateProcess cmd $ F.formatToString F.build (Program insts)
