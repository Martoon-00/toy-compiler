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
import           Control.Monad.Morph        (hoist)
import           Control.Monad.Trans        (lift)
import           Control.Monad.Trans.Either (EitherT (..))
import           Control.Monad.Writer       (WriterT, tell)
import qualified Data.Text                  as T
import qualified Formatting                 as F
import           GHC.Exts                   (toList)

import           Toy.Execution.Data         (Meta, Meta (..))
import           Toy.Execution.Exec         (BinaryFile (..), Executable (..))
import qualified Toy.Lang                   as L
import qualified Toy.SM                     as SM
import qualified Toy.X86                    as X86

data TranslateWay src dist = TranslateWay
    { showTranslateWay :: String
    , translatingIn    :: src -> EitherT String (WriterT [Meta] IO) dist
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

compileX86 :: FilePath -> FilePath -> TranslateWay SM.Insts BinaryFile
compileX86 runtimePath outPath = TranslateWay "SM to binary" $ \insts -> do
    let prog = X86.compile insts
    tell [Meta "Asm" $ F.sformat F.build (X86.Program prog)]
    hoist lift $ EitherT $ X86.produceBinary runtimePath outPath prog
    return (BinaryFile outPath)

defCompileX86 :: TranslateWay SM.Insts BinaryFile
defCompileX86 = compileX86 "./runtime/runtime.o" "./tmp/prog"

data ExecWay l = forall e . Executable e => Ex (TranslateWay l e)

instance Show (ExecWay l) where
    show (Ex way) = show way
