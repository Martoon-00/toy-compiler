{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# OPTIONS_GHC -fno-warn-orphans   #-}

module Toy.Lang.Translator
    ( toIntermediate
    ) where

import           Control.Lens  (Snoc (..), prism, (|>))
import qualified Data.DList    as D
import           Data.Monoid   ((<>))
import qualified Data.Vector   as V

import           Toy.Exp.Data  (Exp (..))
import qualified Toy.Lang.Data as L
import qualified Toy.SM.Data   as SM

instance Snoc (D.DList a) (D.DList a) a a where
    _Snoc = prism (uncurry D.snoc) undefined

toIntermediate :: L.Stmt -> SM.Insts
toIntermediate = V.fromList . D.toList . convert

convert :: L.Stmt -> D.DList SM.Inst
convert L.Skip        = [SM.Nop]
convert (n L.:= e)    = pushExp e |> SM.Store n
convert (L.Read n)    = [SM.Read, SM.Store n]
convert (L.Write e)   = pushExp e |> SM.Write
convert (L.Seq s1 s2) = convert s1 <> convert s2
convert (L.If _ _ _)  = error "Can't translate if :("
convert (L.While _ _) = error "Can't translate while :("

-- | Gives instructions which effectively push  value equals to given
-- expression on stack.
pushExp :: Exp -> D.DList SM.Inst
pushExp (ValueE k)    = [SM.Push k]
pushExp (VarE n)      = [SM.Load n]
pushExp (UnaryE _ _)  = error "SM doesn't support unary operations for now"
pushExp (BinE op a b) = pushExp a <> pushExp b |> SM.Bin op
