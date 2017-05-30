{-# LANGUAGE TemplateHaskell #-}

module Toy.SM.Data where

import           Control.Lens        (makeLenses)
import           Data.Default        (Default (..))
import qualified Data.Map            as M
import           Data.Text.Buildable (Buildable (..))
import qualified Data.Vector         as V
import           Formatting          (bprint, (%))
import qualified Formatting          as F

import           Toy.Base            (BinOp, FunSign (..), Var)
import           Toy.Exp             (ExpRes, LocalVars)

type IP = Int

data LabelId
    = CLabel Int  -- ^ control
    | FLabel Var  -- ^ function
    | ELabel Var  -- ^ function exit, used for correct @return@s in the middle
                  -- of the code
    deriving (Eq, Ord, Show)

instance Buildable LabelId where
    build (CLabel l) = bprint ("L"%F.build) l
    build (FLabel n) = bprint F.build n
    build (ELabel n) = bprint (F.build%"_exit") n

-- | Statement of a program.
data Inst
    = Push ExpRes
    | Drop
    | Dup
    | Bin BinOp
    | Load Var
    | Store Var
    | ArrayMake Int
    | ArrayAccess
    | ArraySet Int
    | Label LabelId
    | Jmp Int
    | JmpIf Int
    | Call FunSign
    | Ret
    | Nop
    | Enter Var [Var]
    deriving (Show)

type Insts = V.Vector Inst

-- | State of execution
data ExecState = ExecState
    { _esLocals :: LocalVars
      -- ^ local variables values
    , _esStack  :: [ExpRes]
      -- ^ current stack
    , _esIp     :: IP
      -- ^ instruction pointer, number of command to execute next
    } deriving (Show)

makeLenses ''ExecState

instance Default ExecState where
    def = ExecState M.empty [] 0

initFunName :: Var
initFunName = "main"

externalFuns :: [FunSign]
externalFuns =
    [ FunSign "read" []
    , FunSign "write" ["x"]
    ]
