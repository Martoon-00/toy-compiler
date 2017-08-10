{-# LANGUAGE OverloadedLists #-}

module Toy.Lang.Translator
    ( toIntermediate
    ) where

import           Control.Applicative        ((<|>))
import           Control.Lens               (ix, (<<+=))
import           Control.Monad              (replicateM)
import           Control.Monad.Error.Class  (throwError)
import           Control.Monad.State        (MonadState)
import           Control.Monad.Trans.Except (ExceptT, runExceptT)
import           Control.Monad.Trans.Maybe  (MaybeT (..))
import           Control.Monad.Trans.RWS    (RWST, evalRWST)
import           Control.Monad.Writer       (listen, pass, tell)
import qualified Data.DList                 as D
import           Data.Foldable              (find)
import qualified Data.Map                   as M
import           Data.Maybe                 (fromJust)
import qualified Data.Set                   as S
import qualified Data.Vector                as V
import           Formatting                 (build, sformat, (%))
import           Universum                  hiding (find, pass)

import           Toy.Base                   (FunSign (..), Var)
import qualified Toy.Constants              as C
import           Toy.Exp.Data               (Exp (..), UserLabelId)
import qualified Toy.Lang.Data              as L
import qualified Toy.SM                     as SM

type LabelsCounter = Int

type TransState =
    RWST L.FunDecls (D.DList SM.Inst) LabelsCounter $
    ExceptT Text Identity

toIntermediate :: L.Program -> Either Text SM.Insts
toIntermediate (L.Program funcs main) = do
    fmap (V.fromList . D.toList) . execTransState funcs $ do
        mapM_ convertFun $ snd <$> M.toList funcs
        convertFun (FunSign SM.initFunName [], main)
  where
    convertFun (FunSign name args, stmt) = do
        tell [SM.Enter name args, SM.Label (SM.FLabel name)]
        bracketLocals $ do
            bracketNonlocalLabels $ do
                convert stmt
                -- could just push on stack, but jump after that would fail due to
                -- SM interpreter laws
                tell [SM.PushNull, SM.Store SM.funResVar, SM.Jmp SM.exitLabel]
            tell [SM.Label SM.exitLabel]
        when (name == SM.initFunName) memCheck
        tell [SM.LoadNoGc SM.funResVar, SM.FunExit]

    initRef :: Var -> D.DList SM.Inst
    initRef var =
        [ SM.StoreInit var ]
    freeRef :: Var -> D.DList SM.Inst
    freeRef var =
        [ SM.Load var, SM.Call $ FunSign "array_free" ["X"], SM.Drop ]

    bracketLocals :: TransState () -> TransState ()
    bracketLocals action
        | C.useGC = pass $ do
            action
            return . pure $ \insts -> do
                let locals = SM.gatherLocals insts
                mconcat
                    [ foldMap initRef locals  -- don't want to clean trash afterwards
                    , insts
                    , foldMap freeRef (S.delete SM.funResVar locals)
                    ]
        | otherwise = action

    memCheck = when C.useGC $
        tell [ SM.Call $ FunSign "ensure_no_allocations" [], SM.Drop ]

    makeTransition :: UserLabelId -> TransState ()
    makeTransition userL =
        tell
            [ SM.Load SM.outLabelVar
            , SM.Push $ fromIntegral userL
            , SM.Bin "=="
            , SM.JmpIf (SM.ULabel userL)
            ]

    bracketNonlocalLabels :: TransState () -> TransState ()
    bracketNonlocalLabels action = pass $ do
        (_, insts) <- listen action
        let ulabels  = SM.gatherLocalULabels insts
        let hasGotos = SM.countOutLabels insts
        (_, table) <- listen $ mapM makeTransition (toList ulabels)
        return . pure . const $
            mconcat $
                [ insts
                , [SM.Label SM.nonlocalLabelsTableLabel]
                ]
            <> if not hasGotos then [] else
                [ [SM.SwitchOutIndicator False]
                , table
                , [SM.Load SM.outLabelVar, SM.Store SM.funResVar]
                , [SM.SwitchOutIndicator True, SM.Jmp SM.exitLabel]
                ]


execTransState :: L.FunDecls -> TransState () -> Either Text (D.DList SM.Inst)
execTransState funcs action =
    runIdentity . runExceptT $ snd <$> evalRWST action funcs 0

convert :: L.Stmt -> TransState ()
convert L.Skip         = tell [SM.Nop]
convert (n L.:= e)     = pushExp e >> tell [SM.Store n]
convert (L.Seq s1 s2)  = convert s1 >> convert s2
convert (L.If c s1 s2) = do
    [midL, endL] <- replicateM 2 genLabel
    pushExp c
    tell [SM.JmpIf midL]
    convert s2
    tell [SM.Jmp endL, SM.Label midL]
    convert s1
    tell [SM.Label endL]
convert (L.DoWhile s c) = do
    label <- genLabel
    tell [SM.Label label]
    convert s
    pushExp c
    tell [SM.JmpIf label]
convert (L.Return e) = pushExp e >> tell [SM.Store SM.funResVar, SM.JumpToFunEnd]
convert (L.ArrayAssign a i e) = do
    pushExp a
    pushExp i
    pushExp e
    tell [SM.ArraySet]
convert (L.Label l) = tell [SM.Label (SM.ULabel l)]
convert (L.Goto e) = do
    pushExp e
    tell [SM.Store SM.outLabelVar, SM.Jmp SM.nonlocalLabelsTableLabel]

genLabel :: MonadState LabelsCounter m => m SM.LabelId
genLabel = SM.CLabel <$> (identity <<+= 1)

-- | Gives instructions which effectively push value, which equals to given
-- expression, on stack.
pushExp :: Exp -> TransState ()
pushExp (ValueE k)    = tell [SM.Push k]
pushExp (VarE n)      = tell [SM.Load n]
pushExp (UnaryE _ _)  = throwError "SM doesn't support unary operations for now"
pushExp (BinE op a b) = do
    pushExp a
    pushExp b
    tell [SM.Bin op]
pushExp (FunE n args) = callFun n args
pushExp (ArrayUninitE k) = tell [SM.ArrayMake k]
pushExp (ArrayAccessE a i) = do
    pushExp a
    pushExp i
    tell [SM.ArrayAccess]
pushExp (LabelE l) = tell [SM.Push $ fromIntegral l]

callFun :: Var -> [Exp] -> TransState ()
callFun name (D.fromList . reverse -> args) = do
    sign <- fmap fromJust . runMaybeT $
            MaybeT (preview (ix name . _1))
        <|> MaybeT (return $ find (\(FunSign n _) -> n == name) SM.externalFuns)
        <|> throwError (sformat ("No such function: "%build) name)

    mapM_ pushExp args
    tell [SM.Call sign]

    tell
        [ SM.TestOutIndicator
        , SM.JmpIf SM.nonlocalLabelsTableLabel
        ]
