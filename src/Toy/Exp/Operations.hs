{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Toy.Exp.Operations where

import           Control.Lens              (has, ix, (%~), (.~), (^?), _Just)
import           Control.Monad.Error.Class (MonadError (..))
import           Control.Monad.Trans       (MonadIO (..))
import           Data.Bits                 (xor, (.&.), (.|.))
import           Data.IORef                (modifyIORef, newIORef, readIORef, writeIORef)
import           Data.String               (IsString (..))
import qualified Data.Vector               as V
import           Formatting                ((%))
import qualified Formatting                as F
import           Universum                 (unless, whenJust, whenNothing, whenNothingM)

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


arrayMakeU :: MonadIO m => Int -> m ExpRes
arrayMakeU k = liftIO $ ArrayR <$> newIORef (Just $ V.replicate k NotInitR)

arrayAccess
    :: (MonadIO m, MonadError s m, IsString s)
    => ExpRes -> ExpRes -> m ExpRes
arrayAccess a i = do
    i' <- pure i `valueOnly` "array access: index is not a number"
    let i'' = fromIntegral i'
    a' <- pure a `arrayOnly` "array access: array expected"
    a'' <- liftIO (readIORef a') `whenNothingM` throwError "array access: value freed"
    (a'' ^? ix i'') `whenNothing` throwError "array access: index out of bounds"

arraySet
    :: (MonadIO m, MonadError s m, IsString s)
    => ExpRes -> ExpRes -> ExpRes -> m ()
arraySet a i e = do
    a'  <- pure a `arrayOnly` "array set: array expected"
    a'' <- liftIO (readIORef a') `whenNothingM` throwError "array set: value freed"
    i'  <- pure i `valueOnly` "array set: array expected"
    let i'' = fromIntegral i'
    unless (ix i'' `has` a'') $
        throwError . fromString $
        F.formatToString ("array set: index out of bounds: "%F.build%
                          " is out of ["%F.build%", "%F.build%")")
                          i'' (0 :: Int) (V.length a'')
    liftIO . modifyIORef a' $ _Just . ix i'' .~ e

arrayLength
    :: (MonadIO m, MonadError s m, IsString s)
    => ExpRes -> m ExpRes
arrayLength a = do
    a' <- pure a `arrayOnly` "array length: array expected"
    arg <- liftIO (readIORef a') `whenNothingM` throwError "array length: value freed"
    return (ValueR . fromIntegral $ V.length arg)

arrayMake
    :: (MonadIO m, MonadError s m, IsString s)
    => ExpRes -> ExpRes -> m ExpRes
arrayMake l e = do
    l' <- pure l `valueOnly` "make array: length should be numeric"
    r  <- liftIO . newIORef . Just $ V.replicate (fromIntegral l') e
    return (ArrayR r)

arrayFree
    :: (MonadIO m, MonadError s m, IsString s)
    => ExpRes -> m ()
arrayFree a =
    -- it's fully ok to to call free on primitive values
    whenJust (a ^? _ArrayR) $ \a' -> do
        _ <- liftIO (readIORef a')
                 `whenNothingM` throwError "array free: already freed"
        liftIO $ writeIORef a' Nothing
