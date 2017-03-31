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

module Test.Execution
    ( TestRes (..)
    , ExecWay (..)
    , BinaryFile (..)
    , (<~~>)
    , asIs
    , translateLang
    , compileX86
    , defCompileX86
    , describeExecWays
    , (>-->)
    , (>-*->)
    , (~~)
    , (~*~)
    ) where

import qualified Control.Category           as Cat
import           Control.Lens               ((%~), (^?), _Left, _Right)
import           Control.Monad              (forM_, (>=>))
import           Control.Monad.Catch        (SomeException, try)
import           Control.Monad.Morph        (hoist)
import           Control.Monad.Trans        (lift)
import           Control.Monad.Trans.Either (EitherT (..))
import           Control.Monad.Writer       (WriterT, runWriterT, tell)
import           Control.Spoon              (teaspoon)
import qualified Data.Map                   as M
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Text.Buildable        (Buildable (..))
import           Formatting                 ((%))
import qualified Formatting                 as F
import           GHC.Exts                   (IsList (..), IsString (..))
import           System.Process             (readProcess)
import           Test.Hspec                 (describe)
import           Test.Hspec.Core.Spec       (SpecWith)
import           Test.QuickCheck            (Arbitrary, NonNegative (..), Property,
                                             counterexample, ioProperty, once, property,
                                             (===))
import           Test.QuickCheck.Property   (failed, reason)

import           Test.Util                  (getOutputValues, parseDataOrFail)
import           Toy.Exp                    (Value)
import qualified Toy.Lang                   as L
import qualified Toy.SM                     as SM
import qualified Toy.X86                    as X86


type In = [Value]
type Out = [Value]
type InOut = (In, Out)

data Meta = Meta
    { metaName :: Text
    , metaBody :: Text
    }

instance Buildable Meta where
    build Meta{..} =
        F.bprint ("\n=== "%F.stext%" ===\n"%F.stext%"\n--^--^--\n")
        metaName metaBody


withEmptyInput :: Out -> InOut
withEmptyInput = ([], )

class Executable e where
    exec :: e -> In -> EitherT String IO InOut

metaCounterexample :: [Meta] -> Property -> Property
metaCounterexample = flip . foldr $ counterexample . F.formatToString F.build

instance Executable L.Stmt where
    exec stmt is =
        let outcome = L.execute $ L.ExecState is [] M.empty stmt
        in  EitherT . return $ L.getIO <$> outcome

instance Executable SM.Insts where
    exec insts is =
        let outcome = SM.execute $ SM.ExecState is [] M.empty [] insts 0
        in  EitherT . return $ SM.getIO <$> outcome

newtype BinaryFile = BinaryFile FilePath
    deriving (Show, Eq, IsString)

instance Executable BinaryFile where
    exec (BinaryFile path) is = do
        let input = unlines (show <$> is)
        -- TODO: extract errors
        output <- grab $ readProcess path [] input
        return . withEmptyInput . getOutputValues . parseDataOrFail $ T.pack output
      where
        showError :: SomeException -> String
        showError = show

        grab = EitherT . fmap (_Left %~ showError) . try


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
translateLang = TranslateWay "Lang to SM" $ return . L.toIntermediate

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

describeExecWays :: [ExecWay l] -> (ExecWay l -> SpecWith a) -> SpecWith a
describeExecWays ways specs = forM_ ways $ describe <$> show <*> specs

propTranslating
    :: ExecWay l
    -> l
    -> (forall e . Executable e => e -> Property)
    -> Property
propTranslating (Ex way) prog testExec = ioProperty $ do
    (eExec, metas) <- runWriterT . runEitherT $ translatingIn way prog
    return $ metaCounterexample metas $
        case eExec of
            Left err -> property failed
                        { reason = "Translation failed: " ++ err }
            Right e  -> testExec e

data TestRes
    = TestRes Out  -- execution produced given output
    | X            -- execution failed

instance IsList TestRes where
    type Item TestRes = Value
    fromList = TestRes
    toList _ = error "toList: impossible for TestRes"

assess :: (Eq a, Show a) => Either String a -> Maybe a -> Property
assess result expected =
    let dispm = maybe "failure" show
        dispe = either ("failure: " ++) show
    in  counterexample
        ("Expected " ++ dispm expected ++ ", got " ++ dispe result)
        (expected == result ^? _Right)

infix 5 >-->
(>-->) :: Executable e => In -> TestRes -> e -> Property
(input >--> res) prog = ioProperty $ do
    outcome <- runEitherT $ exec prog input
    return $ outcome ^? _Right === (withEmptyInput <$> expected res)
  where
    expected (TestRes out) = Just out
    expected X             = Nothing

infix 5 >-*->
(>-*->) :: In -> TestRes -> l -> ExecWay l -> Property
(input >-*-> res) prog way =
    once $ propTranslating way prog $ \executable -> ioProperty $ do
        outcome <- runEitherT $ exec executable input
        return $ outcome `assess` (withEmptyInput <$> expected res)
  where
    expected (TestRes out) = Just out
    expected X             = Nothing

class Extract a p where
    extract :: p -> a

instance Extract a a where
    extract = id

instance Extract a (NonNegative a) where
    extract = getNonNegative

class Equivalence f where
    equivalent :: f -> ([Value] -> EitherT String IO Value) -> [Value] -> Property

instance Equivalence Value where
    equivalent r f0 args = ioProperty $ do
        result <- runEitherT $ f0 (reverse args)
        let expected = teaspoon r
        return $ result `assess` expected

instance (Equivalence f, Extract Value v, Show v, Arbitrary v)
       => Equivalence (v -> f) where
    equivalent f f0 args =
        property $ \arg -> equivalent (f arg) f0 (extract arg : args)


singleOutput :: InOut -> Value
singleOutput ([] , [x]) = x
singleOutput (_:_, _  ) = error "Non empty input remained!"
singleOutput (_,   xs)  = error $ "Non single value in output!: "
                                  ++ show (reverse xs)

-- | Interprets given program in one of our languages as a function, and
-- checks that it's equivalent to another function.
-- Program have to print a single value.
infix 3 ~~
(~~) :: (Equivalence f, Executable e) => f -> e -> Property
f ~~ prog = equivalent f (fmap singleOutput . exec prog) []

-- | Executes given program in our language as a function in given way, and
-- checks that it's equivalent to another function.
-- Program have to print a single value.
infix 3 ~*~
(~*~) :: Equivalence f => f -> l -> ExecWay l -> Property
(f ~*~ prog) way =
    propTranslating way prog $ \executable ->
    equivalent f (fmap singleOutput . exec executable) []
