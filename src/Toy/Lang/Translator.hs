{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

module Toy.Lang.Translator
    ( toIntermediate
    ) where

import           Control.Lens        (Snoc (..), prism, (<<+=), (|>))
import           Control.Monad       (join, replicateM)
import           Control.Monad.State (State, evalState)
import qualified Data.DList          as D
import           Data.Monoid         ((<>))
import qualified Data.Vector         as V

import           Toy.Exp.Data        (Exp (..), Var)
import qualified Toy.Lang.Data       as L
import qualified Toy.SM.Data         as SM

instance Snoc (D.DList a) (D.DList a) a a where
    _Snoc = prism (uncurry D.snoc) undefined

toIntermediate :: L.Stmt -> SM.Insts
toIntermediate = V.fromList . D.toList . flip evalState 0 . convert

convert :: L.Stmt -> State Int (D.DList SM.Inst)
convert L.Skip         = return [SM.Nop]
convert (n L.:= e)     = return $ pushExp e |> SM.Store n
convert (L.Write e)    = return $ pushExp e |> SM.Write
convert (L.Seq s1 s2)  = (<>) <$> convert s1 <*> convert s2
convert (L.If c s1 s2) =
    replicateM 2 genLabel >>= \[midL, endL] -> fmap mconcat $ sequence
        [ pure $ pushExp c
        , pure [SM.JmpIf midL]
        , convert s2
        , pure [SM.Jmp endL, SM.Label midL]
        , convert s1
        , pure [SM.Label endL]
        ]
convert (L.DoWhile s c)  =
    genLabel >>= \label -> fmap mconcat $ sequence
        [ pure [SM.Label label]
        , convert s
        , pure $ pushExp c
        , pure [SM.JmpIf label]
        ]
convert (L.FunCall name args) = return $ callFun name args <> [SM.Pop]

genLabel :: State Int SM.LabelId
genLabel = SM.CLabel <$> (id <<+= 1)

-- | Gives instructions which effectively push value equals to given
-- expression on stack.
pushExp :: Exp -> D.DList SM.Inst
pushExp (ValueE k)    = [SM.Push k]
pushExp (VarE n)      = [SM.Load n]
pushExp ReadE         = [SM.Read]
pushExp (UnaryE _ _)  = error "SM doesn't support unary operations for now"
pushExp (BinE op a b) = pushExp a <> pushExp b |> SM.Bin op
pushExp (Fun n args)  = callFun n args

callFun :: Var -> [Exp] -> D.DList SM.Inst
callFun name (D.fromList . reverse -> args) =
    join (pushExp <$> args)
    <> [SM.Call (SM.FLabel name)]
    <> (SM.Pop <$ args)
