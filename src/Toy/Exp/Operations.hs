{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Toy.Exp.Operations where

import           Control.Lens              (has, ix, (%~), (.~), (^?))
import           Control.Monad.Error.Class (MonadError (..))
import           Control.Monad.Trans       (MonadIO (..))
import           Data.Bits                 (xor, (.&.), (.|.))
import           Data.IORef                (modifyIORef, newIORef, readIORef)
import           Data.String               (IsString)
import qualified Data.Vector               as V
import           Universum                 (unless, whenNothing)

import           Toy.Base                  (BinOp, UnaryOp, Value)
import           Toy.Exp.Data
import           Toy.Exp.Util              (asToBool, binResToBool, bool)

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

binOp op   = error $ "Unsopported operation: " ++ show op


arrayMake :: MonadIO m => Int -> m ExpRes
arrayMake k = liftIO $ ArrayR <$> newIORef (V.replicate k NotInitR)

arrayAccess :: (MonadIO m, MonadError s m, IsString s) => ExpRes -> ExpRes -> m ExpRes
arrayAccess a i = do
    i' <- pure i `valueOnly` "index is not a number"
    let i'' = fromIntegral i'
    a' <- pure a `arrayOnly` "array expected"
    a'' <- liftIO $ readIORef a'
    (a'' ^? ix i'') `whenNothing` throwError "index out of bounds"

arraySet :: (MonadIO m, MonadError s m, IsString s) => ExpRes -> Int -> ExpRes -> m ()
arraySet a i e = do
    a'  <- pure a `arrayOnly` "array expected"
    a'' <- liftIO $ readIORef a'
    unless (ix i `has` a'') $ throwError "index out of bounds"
    liftIO . modifyIORef a' $ ix i .~ e
