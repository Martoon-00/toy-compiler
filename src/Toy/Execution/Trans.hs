{-# LANGUAGE ExistentialQuantification #-}

module Toy.Execution.Trans
    ( TranslateWay (..)
    , (<~~>)
    , asIs
    , transShow

    , ExecWay (..)
    , translateLang
    , compileX86
    , defCompileX86

    , TranslateToSM

    , printMetaSM
    ) where

import qualified Control.Category           as Cat
import           Control.Monad              ((>=>))
import           Control.Monad.Morph        (hoist)
import           Control.Monad.Trans        (lift)
import           Control.Monad.Writer       (Writer, tell)
import           Data.Functor               (($>))
import           Data.Functor.Contravariant (Contravariant (..))
import           Data.Profunctor            (Profunctor (..))
import qualified Data.Text                  as T
import           Data.Text.Buildable        (Buildable (..))
import qualified Formatting                 as F
import qualified Prelude
import           System.IO.Unsafe           (unsafeInterleaveIO, unsafePerformIO)
import           Universum

import           Toy.Execution.Data         (Meta, Meta (..))
import           Toy.Execution.Exec         (BinaryFile (..), Executable (..))
import qualified Toy.Lang                   as L
import qualified Toy.SM                     as SM
import qualified Toy.X86                    as X86

data TranslateWay src dist = TranslateWay
    { showTranslateWay :: Text
    , translatingIn    :: src -> ExceptT Text (Writer [Meta]) dist
    }

instance Buildable (TranslateWay a b) where
    build TranslateWay{..} = build showTranslateWay

instance Show (TranslateWay a b) where
    show = F.formatToString F.build

instance Profunctor TranslateWay where
    dimap f g (TranslateWay d t) = TranslateWay d (fmap g . t . f)

instance Functor (TranslateWay src) where
    fmap = rmap

(<~~>) :: TranslateWay a b -> TranslateWay b c -> TranslateWay a c
tw1 <~~> tw2 =
    TranslateWay (showTranslateWay tw1 <> " ~> " <> showTranslateWay tw2)
                 (translatingIn tw1 >=> translatingIn tw2)

asIs :: TranslateWay a a
asIs = TranslateWay "Interpret" return

transShow :: Show a => TranslateWay a a
transShow = TranslateWay "Interpret" $ \a ->
    tell [Meta "Origin" $ F.sformat F.shown a] $> a

instance Cat.Category TranslateWay where
    id = asIs
    (.) = flip (<~~>)

class TranslateToSM l where
    translateLang :: TranslateWay l SM.Insts

printMetaSM :: TranslateWay SM.Insts SM.Insts
printMetaSM = TranslateWay "Log" $ \insts -> do
    tell [Meta "SM" $ T.unlines $ T.pack . show <$> toList insts]
    return insts

instance TranslateToSM L.Program where
    translateLang = TranslateWay "Lang to SM" $ \orig -> do
        tell [Meta "Lang" $ F.sformat F.shown orig]
        prog <- hoist lift $ ExceptT . return $ L.toIntermediate orig
        translatingIn printMetaSM prog

instance TranslateToSM L.Stmt where
    translateLang = lmap L.toProgram translateLang

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
    ExceptT . return $ mkBinaryUnsafe X86.produceBinary runtimePath outPath prog

defCompileX86 :: TranslateWay SM.Insts BinaryFile
defCompileX86 = compileX86 "./runtime/" "./tmp/prog"

data ExecWay l = forall e . Executable e => Ex (TranslateWay l e)

instance Buildable (ExecWay l) where
    build (Ex way) = build way

instance Show (ExecWay l) where
    show = F.formatToString F.build

instance Contravariant ExecWay where
    contramap f (Ex t) = Ex (lmap f t)

