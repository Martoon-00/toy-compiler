{-# LANGUAGE ExplicitNamespaces    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

module Toy.Lang.Translator
    ( toIntermediate
    ) where

import           Control.Applicative  ((<|>))
import           Control.Lens         (Snoc (..), ix, preview, prism, (<<+=), _1)
import           Control.Monad        (replicateM)
import           Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import           Control.Monad.State  (MonadState, State, evalState)
import qualified Data.DList           as D
import           Data.Foldable        (find, fold)
import qualified Data.Map             as M
import           Data.Maybe           (fromMaybe)
import           Data.Monoid          ((<>))
import qualified Data.Vector          as V
import           Formatting           (build, formatToString, (%))
import           Universum            (type ($))

import           Toy.Exp.Data         (Exp (..), FunSign (..), Var)
import qualified Toy.Lang.Data        as L
import qualified Toy.SM.Data          as SM

instance Snoc (D.DList a) (D.DList a) a a where
    _Snoc = prism (uncurry D.snoc) undefined

instance Traversable D.DList where
    traverse f l = fmap D.fromList $ traverse f (D.toList l)

toIntermediate :: L.Program -> SM.Insts
toIntermediate (L.Program funcs main) =
    V.fromList . D.toList . flip evalState 0 . flip runReaderT funcs $ do
        funcsC <- mapM convertFun $ snd <$> M.toList funcs
        mainC  <- convertFun (FunSign SM.initFunName [], main)
        return $ mconcat funcsC <> mainC
  where
    convertFun (FunSign name args, stmt) = fmap mconcat $ sequence
        [ pure [ SM.Enter name args, SM.Label $ SM.FLabel name ]
        , convert stmt
        , pure [ SM.Push 0, SM.Ret, SM.Label (SM.ELabel name) ]
        ]

convert :: L.Stmt -> ReaderT L.FunDecls (State Int) $ D.DList SM.Inst
convert L.Skip         = return [SM.Nop]
convert (n L.:= e)     = fmap mconcat $ sequence
    [ pushExp e
    , pure [SM.Store n]
    ]
convert (L.Seq s1 s2)  = (<>) <$> convert s1 <*> convert s2
convert (L.If c s1 s2) =
    replicateM 2 genLabel >>= \[midL, endL] -> fmap mconcat $ sequence
        [ pushExp c
        , pure [SM.JmpIf midL]
        , convert s2
        , pure [SM.Jmp endL, SM.Label $ SM.CLabel midL]
        , convert s1
        , pure [SM.Label $ SM.CLabel endL]
        ]
convert (L.DoWhile s c)  =
    genLabel >>= \label -> fmap mconcat $ sequence
        [ pure [SM.Label $ SM.CLabel label]
        , convert s
        , pushExp c
        , pure [SM.JmpIf label]
        ]
convert (L.FunCall (name, args)) = fmap mconcat $ sequence
    [ callFun name args
    , pure [SM.Drop]
    ]
convert (L.Return e) = fmap mconcat $ sequence
    [ pushExp e
    , pure [SM.Ret]
    ]

genLabel :: MonadState Int m => m Int
genLabel = id <<+= 1

-- | Gives instructions which effectively push value equals to given
-- expression on stack.
pushExp :: MonadReader L.FunDecls m => Exp -> m (D.DList SM.Inst)
pushExp (ValueE k)    = return [SM.Push k]
pushExp (VarE n)      = return [SM.Load n]
pushExp (UnaryE _ _)  = error "SM doesn't support unary operations for now"
pushExp (BinE op a b) = fmap mconcat $ sequence
    [ pushExp a
    , pushExp b
    , pure [SM.Bin op]
    ]
pushExp (FunE (n, args)) = callFun n args

-- TODO: nice error processing
callFun :: MonadReader L.FunDecls m => Var -> [Exp] -> m (D.DList SM.Inst)
callFun name (D.fromList . reverse -> args) = do
    msign <- preview (ix name . _1)
    let mesign = find (\(FunSign n _) -> n == name) SM.externalFuns
        sign = fromMaybe (error $ formatToString ("No such function: "%build) name) $
               msign <|> mesign
    exps <- fold <$> mapM pushExp args
    return $ exps <> [SM.Call sign]
