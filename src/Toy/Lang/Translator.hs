module Toy.Lang.Translator
    ( toIntermediate
    ) where

import qualified Data.DList    as D
import           Data.Monoid   ((<>))
import qualified Data.Vector   as V

import           Toy.Data      (Exp (..))
import qualified Toy.Lang.Data as L
import qualified Toy.SM.Data   as SM

toIntermediate :: L.Stmt -> V.Vector SM.Inst
toIntermediate = V.fromList . D.toList . convert

convert :: L.Stmt -> D.DList SM.Inst
convert L.Skip        = pure SM.Nop
convert (n L.:= e)    = pushExp e <> pure (SM.St n)
convert (L.Read n)    = pure SM.Read <> pure (SM.St n)
convert (L.Write e)   = pushExp e <> pure SM.Write
convert (L.Seq s1 s2) = convert s1 <> convert s2
convert (L.If _ _ _)  = error "Can't translate if :("
convert (L.While _ _) = error "Can't translate while :("

-- | Gives instructions which effectively pushes given expression on stack.
pushExp :: Exp -> D.DList SM.Inst
pushExp (ValueE k)    = pure $ SM.Push k
pushExp (VarE n)      = pure $ SM.Ld n
pushExp (UnaryE _ _)  = error "SM doesn't support unary operations for now"
pushExp (BinE op a b) = mconcat [pushExp a, pushExp b, pure (SM.Bin op)]
