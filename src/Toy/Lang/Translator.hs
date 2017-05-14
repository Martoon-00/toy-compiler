{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

module Toy.Lang.Translator
    ( toIntermediate
    ) where

import           Control.Applicative        ((<|>))
import           Control.Lens               (Snoc (..), ix, preview, prism, (<<+=), _1)
import           Control.Monad              (replicateM)
import           Control.Monad.Error.Class  (throwError)
import           Control.Monad.State        (MonadState)
import           Control.Monad.Trans.Except (ExceptT, runExceptT)
import           Control.Monad.Trans.Maybe  (MaybeT (..))
import           Control.Monad.Trans.RWS    (RWST, evalRWST)
import           Control.Monad.Writer       (tell)
import qualified Data.DList                 as D
import           Data.Foldable              (find)
import qualified Data.Map                   as M
import           Data.Maybe                 (fromJust)
import           Data.Maybe                 (fromMaybe)
import qualified Data.Vector                as V
import           Formatting                 (build, formatToString, (%))
import           Universum                  (type ($), Identity (..))

import           Toy.Exp.Data               (Exp (..), FunSign (..), Var)
import qualified Toy.Lang.Data              as L
import qualified Toy.SM.Data                as SM

instance Snoc (D.DList a) (D.DList a) a a where
    _Snoc = prism (uncurry D.snoc) undefined

instance Traversable D.DList where
    traverse f l = fmap D.fromList $ traverse f (D.toList l)

type TransState = RWST L.FunDecls (D.DList SM.Inst) Int $ ExceptT String Identity

execTransState :: L.FunDecls -> TransState () -> Either String (D.DList SM.Inst)
execTransState funcs action = runIdentity . runExceptT $ snd <$> evalRWST action funcs 0

toIntermediate :: L.Program -> Either String SM.Insts
toIntermediate (L.Program funcs main) =
    fmap (V.fromList . D.toList) . execTransState funcs $ do
        mapM_ convertFun $ snd <$> M.toList funcs
        convertFun (FunSign SM.initFunName [], main)
  where
    convertFun (FunSign name args, stmt) = do
        tell [ SM.Enter name args, SM.Label $ SM.FLabel name ]
        convert stmt
        tell [ SM.Push 0, SM.Ret, SM.Label (SM.ELabel name) ]

convert :: L.Stmt -> TransState ()
convert L.Skip         = tell [SM.Nop]
convert (n L.:= e)     = pushExp e >> tell [SM.Store n]
convert (L.Seq s1 s2)  = convert s1 >> convert s2
convert (L.If c s1 s2) = do
    [midL, endL] <- replicateM 2 genLabel
    pushExp c
    tell [SM.JmpIf midL]
    convert s2
    tell [SM.Jmp endL, SM.Label $ SM.CLabel midL]
    convert s1
    tell [SM.Label $ SM.CLabel endL]
convert (L.DoWhile s c) = do
    label <- genLabel
    tell [SM.Label $ SM.CLabel label]
    convert s
    pushExp c
    tell [SM.JmpIf label]
convert (L.FunCall (name, args)) = callFun name args >> tell [SM.Drop]
convert (L.Return e) = pushExp e >> tell [SM.Ret]

genLabel :: MonadState Int m => m Int
genLabel = id <<+= 1

-- | Gives instructions which effectively push value equals to given
-- expression on stack.
pushExp :: Exp -> TransState ()
pushExp (ValueE k)    = tell [SM.Push k]
pushExp (VarE n)      = tell [SM.Load n]
pushExp (UnaryE _ _)  = throwError "SM doesn't support unary operations for now"
pushExp (BinE op a b) = do
    pushExp a
    pushExp b
    tell [SM.Bin op]
pushExp (FunE (n, args)) = callFun n args

callFun :: Var -> [Exp] -> TransState ()
callFun name (D.fromList . reverse -> args) = do
    sign <- fmap fromJust . runMaybeT $
            MaybeT (preview (ix name . _1))
        <|> MaybeT (return $ find (\(FunSign n _) -> n == name) SM.externalFuns)
        <|> throwError (formatToString ("No such function: "%build) name)

    mapM_ pushExp args
    tell [SM.Call sign]
