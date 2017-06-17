{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Toy.Exp.Operations where

import           Control.Lens              (has, ix, (.=))
import           Control.Monad.Error.Class (MonadError (..))
import           Control.Monad.State       (get)
import           Control.Monad.Trans       (MonadIO (..))
import           Data.Bits                 (xor, (.&.), (.|.))
import           Data.String               (IsString (..))
import qualified Data.Vector               as V
import           Formatting                ((%))
import qualified Formatting                as F
import           Universum

import           Toy.Base                  (BinOp, UnaryOp, Value)
import           Toy.Exp.Arrays            (arrayOnly, initArray, valueOnly)
import           Toy.Exp.Data
import           Toy.Exp.RefEnv            (MonadRefEnv (..), MonadRefInit)
import           Toy.Exp.Util              (asToBool, binResToBool, boolL)

-- * Unary operations

notE :: Exp -> Exp
notE  = UnaryE "!"

unaryOp :: UnaryOp -> Value -> Value
unaryOp "!" = boolL %~ not
unaryOp op  = error $ "Unsupported operation: " <> show op

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

infix 9 !!:

(+:), (-:), (*:), (/:), (%:),
    (&&:), (||:), (^:), (&:), (|:),
    (<:), (>:), (<=:), (>=:), (==:), (!=:),
    (!!:)
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

(!!:) = ArrayAccessE

binOp :: BinOp -> Value -> Value -> Value
binOp "+"  = (+)
binOp "-"  = (-)
binOp "*"  = (*)
binOp "/"  = quot
binOp "%"  = rem

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

binOp op   = error $ "Unsopported operation: " <> show op


type MonadArrays s m =
    ( MonadIO m
    , MonadError s m
    , IsString s
    , MonadRefInit m
    , MonadRefEnv ExpRes m
    )

arrayMakeU
    :: (MonadIO m, MonadRefInit m, MonadRefEnv ExpRes m)
    => Int -> m ExpRes
arrayMakeU k = initArray $ V.replicate k NotInitR

arrayAccess
    :: (MonadArrays s m)
    => ExpRes -> ExpRes -> m ExpRes
arrayAccess a i = do
    i' <- pure i `valueOnly` "array access: index is not a number"
    let i'' = fromIntegral i'
    a' <- arrayOnly a "array access: array expected" get
    res <- (a' ^? ix i'') `whenNothing` throwError "array access: index out of bounds"
    changeRefCounter (-) a
    changeRefCounter (+) res
    return res

arraySet
    :: (MonadIO m, MonadError s m, IsString s, MonadRefEnv ExpRes m)
    => ExpRes -> ExpRes -> ExpRes -> m ()
arraySet a i e = do
    a'  <- arrayOnly a "array set: array expected" get
    i'  <- pure i `valueOnly` "array set: array expected"
    let i'' = fromIntegral i'
    unless (ix i'' `has` a') $
        throwError . fromString $
        F.formatToString ("array set: index out of bounds: "%F.build%
                          " is out of ["%F.build%", "%F.build%")")
                          i'' (0 :: Int) (V.length a')
    arrayOnly a "?" (ix i'' .= e)
    changeRefCounter (-) a

arrayLength
    :: (MonadIO m, MonadError s m, IsString s, MonadRefEnv ExpRes m)
    => ExpRes -> m ExpRes
arrayLength a = do
    a' <- arrayOnly a "array length: array expected" get
    changeRefCounter (-) a
    return (ValueR . fromIntegral $ V.length a')

arrayMake
    :: MonadArrays s m
    => ExpRes -> ExpRes -> m ExpRes
arrayMake l e = do
    l' <- pure l `valueOnly` "make array: length should be numeric"
    initArray $ V.replicate (fromIntegral l') e

arrayFree
    :: MonadArrays s m
    => ExpRes -> m ()
arrayFree = changeRefCounter (-)
