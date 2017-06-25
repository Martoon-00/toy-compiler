module Test.Arbitrary where

import           Control.Monad   (liftM2)
import           Data.String     (fromString)
import           Test.QuickCheck (Arbitrary (..), choose, elements, frequency, getSmall)
import           Universum

import           Toy.Base        (Value (..), Var (..))
import           Toy.Exp
import           Toy.Lang        (Stmt (..))
import qualified Toy.Lang        as L
import           Toy.Util        (clearPHBit)

instance Arbitrary Var where
    arbitrary = fromString . pure <$> choose ('a', 'z')

instance Arbitrary Value where
    arbitrary = Value . clearPHBit <$> arbitrary

instance Arbitrary Exp where
    arbitrary = frequency
        [ (50, ValueE . getSmall <$> arbitrary)
        , (10, pure readE)

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
        , (2, L.writeS <$> arbitrary)
        , (1, If <$> arbitrary <*> arbitrary <*> arbitrary)
        , (1, forLoop <$> (Var . toText . (:"_i") <$> choose ('a', 'z'))
                      <*> elements [0, 1] <*> arbitrary)
        , (4, Seq <$> arbitrary <*> arbitrary)
        , (8, pure Skip)
        ]
      where
        forLoop i n body = mconcat
            [ i := 0
            , L.whileS (VarE i <=: n) $ mconcat
                [ body
                , i := VarE i + 1
                ]
            ]
