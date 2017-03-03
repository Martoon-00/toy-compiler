module Test.Arbitrary where

import           Control.Monad   (liftM2)
import qualified Data.Map        as M
import           Data.String     (fromString)
import           Test.QuickCheck (Arbitrary (..), choose, frequency, getSmall, vector)

import           Toy.Exp.Data
import           Toy.Lang.Data   (ExecState (..), Stmt (..))

instance Arbitrary Var where
    arbitrary = fromString . pure <$> choose ('a', 'z')

instance Arbitrary Exp where
    arbitrary = frequency
        [ (50, ValueE . getSmall <$> arbitrary)

        , (1, liftM2 (+:) arbitrary arbitrary)
        , (1, liftM2 (-:) arbitrary arbitrary)
        , (1, liftM2 (*:) arbitrary arbitrary)
        , (1, liftM2 (/:) arbitrary arbitrary)
        , (1, liftM2 (%:) arbitrary arbitrary)

        , (1, notE <$> arbitrary)
        , (1, liftM2 (&&:) arbitrary arbitrary)
        , (1, liftM2 (||:) arbitrary arbitrary)
        , (1, liftM2 (^:) arbitrary arbitrary)
        , (1, liftM2 (&:) arbitrary arbitrary)
        , (1, liftM2 (|:) arbitrary arbitrary)

        , (1, liftM2 (>:) arbitrary arbitrary)
        , (1, liftM2 (<:) arbitrary arbitrary)
        , (1, liftM2 (>=:) arbitrary arbitrary)
        , (1, liftM2 (<=:) arbitrary arbitrary)
        , (1, liftM2 (==:) arbitrary arbitrary)
        , (1, liftM2 (!=:) arbitrary arbitrary)
        ]

instance Arbitrary Stmt where
    arbitrary = frequency
        [ (3, liftM2 (:=) arbitrary arbitrary)
        , (2, Read <$> arbitrary)
        , (2, Write <$> arbitrary)
        , (1, If <$> arbitrary <*> arbitrary <*> arbitrary)
        -- , WhileS  -- TODO
        , (4, Seq <$> arbitrary <*> arbitrary)
        , (8, pure Skip)
        ]

instance Arbitrary ExecState where
    arbitrary = ExecState <$> vector 0 <*> pure [] <*> pure M.empty <*> arbitrary
