{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Toy.Exp.Operations where

import           Control.Lens ((%~))
import           Data.Bits    (xor, (.&.), (.|.))

import           Toy.Exp.Data
import           Toy.Exp.Util (asToBool, binResToBool, bool)

-- * Unary operations

notE :: Exp -> Exp
notE  = UnaryE "!"

unaryOp :: UnaryOp -> Value -> Value
unaryOp "!" = bool %~ not
unaryOp op  = error $ "Unsupported operation: " ++ show op

-- * Binary operations

infixl 6 +:
infixl 6 -:
infixl 7 *:
infixl 7 /:
infixl 7 %:

infixr 3 &&:
infixr 2 ||:
infixl 7 &:
infixl 5 |:
infixl 6 ^:

infix 4 <:
infix 4 <=:
infix 4 >:
infix 4 >=:
infix 4 ==:
infix 4 !=:

(+:), (-:), (*:), (/:), (%:),
    (&&:), (||:), (^:), (&:), (|:),
    (<:), (>:), (<=:), (>=:), (==:), (!=:)
    :: Exp -> Exp -> Exp

(+:)  = BinE "+"
(-:)  = BinE "-"
(*:)  = BinE "*"
(/:)  = BinE "/"
(%:)  = BinE "%"

(&&:) = BinE "&&"
(||:) = BinE "||"
(^:)  = BinE "^"
(&:)  = BinE "&"
(|:)  = BinE "|"

(<:)  = BinE "<"
(>:)  = BinE ">"
(<=:) = BinE "<="
(>=:) = BinE ">="
(==:) = BinE "=="
(!=:) = BinE "!="

binOp :: BinOp -> Value -> Value -> Value
binOp "+"  = (+)
binOp "-"  = (-)
binOp "*"  = (*)
binOp "/"  = div
binOp "%"  = mod

binOp "&&" = asToBool (&&)
binOp "||" = asToBool (||)
binOp "^"  = xor
binOp "&"  = (.&.)
binOp "|"  = (.|.)

binOp ">"  = binResToBool (>)
binOp ">=" = binResToBool (>=)
binOp "<"  = binResToBool (<)
binOp "<=" = binResToBool (<=)
binOp "==" = binResToBool (==)
binOp "!=" = binResToBool (/=)

binOp op   = error $ "Unsopported operation: " ++ show op
