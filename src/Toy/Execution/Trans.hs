{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

module Toy.Execution.Trans
    ( TranslateWay (..)
    , (<~~>)
    , asIs

    , ExecWay (..)
    , translateLang
    , compileX86
    , defCompileX86
    ) where

import qualified Control.Category           as Cat
import           Control.Monad              ((>=>))
import           Control.Monad.Trans.Either (EitherT (..))
import           Control.Monad.Writer       (Writer, tell)
import           Data.Functor               (($>))
import qualified Data.Text                  as T
import qualified Formatting                 as F
import           GHC.Exts                   (toList)
import           System.IO.Unsafe           (unsafeInterleaveIO, unsafePerformIO)

import           Toy.Execution.Data         (Meta, Meta (..))
import           Toy.Execution.Exec         (BinaryFile (..), Executable (..))
import qualified Toy.Lang                   as L
import qualified Toy.SM                     as SM
import qualified Toy.X86                    as X86

data TranslateWay src dist = TranslateWay
    { showTranslateWay :: String
    , translatingIn    :: src -> EitherT String (Writer [Meta]) dist
    }

instance Show (TranslateWay a b) where
    show TranslateWay{..} = showTranslateWay

(<~~>) :: TranslateWay a b -> TranslateWay b c -> TranslateWay a c
tw1 <~~> tw2 =
    TranslateWay (showTranslateWay tw1 ++ " ~> " ++ showTranslateWay tw2)
                 (translatingIn tw1 >=> translatingIn tw2)

asIs :: TranslateWay a a
asIs = TranslateWay "Interpret" return

instance Cat.Category TranslateWay where
    id = asIs
    (.) = flip (<~~>)

translateLang :: TranslateWay L.Stmt SM.Insts
translateLang = TranslateWay "Lang to SM" $ \orig -> do
    let prog = L.toIntermediate orig
    tell [Meta "SM" $ T.unlines $ T.pack . show <$> toList prog]
    return prog

mkBinaryUnsafe
    :: (Functor f)
    => (FilePath -> FilePath -> a -> IO (f ()))
    -> FilePath -> FilePath -> a -> f BinaryFile
mkBinaryUnsafe compiler runtimePath outputPath prog =
    unsafePerformIO . unsafeInterleaveIO $ do
        env <- compiler runtimePath outputPath prog
        return $ env $> BinaryFile outputPath
{-# NOINLINE mkBinaryUnsafe #-}

compileX86 :: FilePath -> FilePath -> TranslateWay SM.Insts BinaryFile
compileX86 runtimePath outPath = TranslateWay "SM to binary" $ \insts -> do
    let prog = X86.compile insts
    tell [Meta "Asm" $ F.sformat F.build (X86.Program prog)]
    EitherT . return $ mkBinaryUnsafe X86.produceBinary runtimePath outPath prog

defCompileX86 :: TranslateWay SM.Insts BinaryFile
defCompileX86 = compileX86 "./runtime/" "./tmp/prog"

data ExecWay l = forall e . Executable e => Ex (TranslateWay l e)

instance Show (ExecWay l) where
    show (Ex way) = show way