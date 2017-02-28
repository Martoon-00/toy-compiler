module Test.Util
    ( running
    , TestInOut (..)
    ) where

import           Control.Lens         ((^?), _Right)
import qualified Data.Map             as M
import           Test.QuickCheck      (Property, (===))

import           Toy.Data             (Value)
import           Toy.Lang.Data        (ExecState (..), Stmt, getIO)
import           Toy.Lang.Interpreter (execute)

type In = [Value]
type Out = [Value]
type InOut = ([Value], [Value])

class Executable e where
    exec :: e -> In -> Maybe InOut

instance Executable Stmt where
    exec stmt is = getIO <$> execute (ExecState is [] M.empty stmt) ^? _Right

data TestInOut
    = In :~~> Out
    | In :~~% ()

infix 1 :~~>
infix 1 :~~%

running :: Executable e => e -> TestInOut -> Property
running prog (is :~~> os) = exec prog is === Just ([], os)
running prog (is :~~% _ ) = exec prog is === Nothing
