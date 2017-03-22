{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

module Test.Execution
    ( TestRes (..)
    , ExecWay (..)
    , BinaryFile (..)
    , defCompileWay
    , describeExecWays
    , (>-->)
    , (>-*->)
    , (~~)
    , (~*~)
    ) where

import           Control.Lens               ((%~), (^?), _Left, _Right)
import           Control.Monad              (forM_)
import           Control.Monad.Catch        (SomeException, try)
import           Control.Monad.Trans.Either (EitherT (..))
import           Control.Spoon              (teaspoon)
import qualified Data.Map                   as M
import qualified Data.Text                  as T
import           GHC.Exts                   (IsList (..), IsString (..))
import           System.IO.Unsafe           (unsafeInterleaveIO, unsafePerformIO)
import           System.Process             (readProcess)
import           Test.Hspec                 (describe)
import           Test.Hspec.Core.Spec       (SpecWith)
import           Test.QuickCheck            (Arbitrary, NonNegative (..), Property,
                                             counterexample, ioProperty, property, (===))

import           Test.Util                  (getOutputValues, parseDataOrFail)
import           Toy.Exp                    (Value)
import qualified Toy.Lang                   as L
import qualified Toy.SM                     as SM
import qualified Toy.X86                    as X86

type In = [Value]
type Out = [Value]
type InOut = ([Value], [Value])

withEmptyInput :: Out -> InOut
withEmptyInput = ([], )

class Executable e where
    exec :: e -> In -> EitherT String IO InOut

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

-- | Wow wow, lazy IO!
--
-- Makes given compiler IO-function pure.
-- Once produced link to file is used, actual compilation occurs and
-- binary gets created.
--
-- This is unsafe in a sense, that if 2 binaries are created with this
-- function call, the one which overwrites (generated last) is not the one
-- which was produced with last function call, but which was actually used
-- last.
mkBinaryUnsafe
    :: (FilePath -> FilePath -> a -> IO ())
    -> FilePath -> FilePath -> a -> BinaryFile
mkBinaryUnsafe compiler runtimePath outputPath prog =
    unsafePerformIO . unsafeInterleaveIO $ do
        compiler runtimePath outputPath prog
        return $ BinaryFile outputPath
{-# NOINLINE mkBinaryUnsafe #-}

instance Executable BinaryFile where
    exec (BinaryFile path) is = do
        let input = unlines (show <$> is)
        output <- grab $ readProcess path [] input
        return . withEmptyInput . getOutputValues . parseDataOrFail $ T.pack output
      where
        showError :: SomeException -> String
        showError = show

        grab = EitherT . fmap (_Left %~ showError) . try

data ExecWay
    = Interpret
    -- ^ Interpret language directly
    | Translate
    -- ^ Translate to SM and interpret
    | Compile FilePath FilePath
    -- ^ Compile to asm and execute.
    -- 1st argument is runtime path, 2nd - output binary name.
    deriving (Eq)

instance Show ExecWay where
    show Interpret     = "Lang interpreter"
    show Translate     = "Translator + SM interpreter"
    show (Compile _ _) = "Compiler + execution"

defCompileWay :: ExecWay
defCompileWay = Compile "./runtime/runtime.o" "./tmp/prog"

inExecWay :: ExecWay -> L.Stmt -> In -> EitherT String IO InOut
inExecWay Interpret = exec
inExecWay Translate = exec . L.toIntermediate
inExecWay (Compile runtimePath progPath) =
    exec . mkBinaryUnsafe X86.produceBinary runtimePath progPath
         . X86.compile . L.toIntermediate

describeExecWays :: [ExecWay] -> (ExecWay -> SpecWith a) -> SpecWith a
describeExecWays ways specs = forM_ ways $ describe <$> show <*> specs


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
(>-*->) :: In -> TestRes -> L.Stmt -> ExecWay -> Property
(input >-*-> res) prog way = ioProperty $ do
    outcome <- runEitherT $ inExecWay way prog input
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
(~*~) :: Equivalence f => f -> L.Stmt -> ExecWay -> Property
(f ~*~ prog) way = equivalent f (fmap singleOutput . inExecWay way prog) []
