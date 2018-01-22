{-# LANGUAGE TemplateHaskell #-}

module Toy.SM.Data where

import           Control.Lens        (makeLenses)
import           Data.Default        (Default (..))
import qualified Data.Map            as M
import           Data.Text.Buildable (Buildable (..))
import qualified Data.Vector         as V
import           Formatting          (bprint, (%))
import qualified Formatting          as F
import           Universum

import           Toy.Base            (BinOp, FunSign (..), Value, Var (..),
                                      stdFunExamples)
import           Toy.Exp             (ExpRes, LocalVars)

type IP = Int

data LabelId
    = CLabel Int  -- ^ control
    | FLabel Var  -- ^ function
    | LLabel Int  -- ^ function-local labels
    deriving (Eq, Ord, Show)

instance Buildable LabelId where
    build (CLabel l) = bprint ("L"%F.build) l
    build (FLabel n) = bprint F.build n
    build (LLabel i) = bprint F.build i

newtype JmpLabelForm = JmpLabelForm LabelId

instance Buildable JmpLabelForm where
    build (JmpLabelForm (LLabel i)) = bprint (F.build%"f") i  -- curently used for 'Ret' only
    build (JmpLabelForm label)      = bprint F.build label

-- | Statement of a program.
data Inst
    = Push Value
    | PushNull
    | Drop
    | Dup
    | Bin BinOp
    | Load Var
    | LoadNoGc Var
    | Store Var
    | StoreInit Var  -- sets local to 0 without doing gc to old stored value
    | ArrayMake
    | ArrayAccess
    | ArraySet
    | Label LabelId
    | Jmp LabelId
    | JmpIf LabelId
    | Call FunSign
    | JumpToFunEnd
    | FunExit
    | Nop
    | Enter Var [Var]  -- ^ function start indicator with fun name and args
    deriving (Show, Eq)

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
    stdFunExamples <&> \(name, args) ->
        FunSign name $ zipWith const (Var . one <$> ['a'..'z']) args


-- * Function local stuff

exitLabel :: LabelId
exitLabel = LLabel 10  -- make them memorable, right?

funResVar :: Var
funResVar = "_res"
