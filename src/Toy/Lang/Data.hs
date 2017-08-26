{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Toy.Lang.Data where

import           Control.Lens              (makeLenses, makePrisms)
import           Control.Monad.Error.Class (MonadError (..))
import           Control.Monad.Reader      (MonadReader)
import           Control.Monad.State       (MonadState)
import           Control.Monad.Trans       (MonadIO)
import           Data.Default              (def)
import qualified Data.Map                  as M
import           Data.Monoid               ((<>))
import           Data.String               (IsString (..))
import           Formatting                (sformat, sformat, shown, stext, (%))
import           GHC.Exts                  (IsList (..))
import           Universum                 hiding (toList)

import           Toy.Base                  (FunName, FunSign (..), Var (..))
import           Toy.Exp                   (Exp (..), ExpRes, LocalVars, MonadRefEnv,
                                            UserLabelId)
import           Toy.Util.Error            (mapError)

-- | Statement of a program.
data Stmt
    = Var := Exp
    | If Exp Stmt Stmt
    | DoWhile Stmt Exp  -- ^ @do .. while@ is the most optimal / easy loop from
                        -- asm point of view
    | Return Exp
    | ArrayAssign Exp Exp Exp  -- ^ array, index and value to assign
    | Seq Stmt Stmt
    | Skip
    | Label UserLabelId
    | Goto Exp
    deriving (Show)

infix 2 :=

instance Monoid Stmt where
    mempty = Skip
    mappend = Seq

type FunDecl = (FunSign, Stmt)

type FunDecls = M.Map FunName FunDecl

toFunDecls :: FunName -> [Var] -> Stmt -> FunDecls
toFunDecls name args body = one (name, (FunSign name args, body))

mainToFunDecls :: Stmt -> FunDecls
mainToFunDecls = toFunDecls def []

-- | Builds function declarations map
mkFunDecls :: (IsList l, Item l ~ FunDecl) => l -> FunDecls
mkFunDecls = fromList . map doLol . toList
  where
    doLol d@(FunSign n _, _) = (n, d)

data Program = Program
    { getProgram :: FunDecls
    } deriving (Show)

toProgram :: Stmt -> Program
toProgram = Program . mainToFunDecls

mkProgram :: FunDecls -> Stmt -> Program
mkProgram decls main = Program $ decls <> mainToFunDecls main

data Branch
    = LeftPath
    | RightPath
    deriving (Eq, Show)

type StmtCoord = [Branch]

data StmtFunCoord = StmtFunCoord
    { sfcFun   :: FunName
    , sfcCoord :: StmtCoord
    } deriving (Eq, Show)

data ExecInterrupt
    = Error Text           -- ^ Execution exception
    | Returned ExpRes      -- ^ Function returns
    | Jumped StmtFunCoord  -- ^ Jumped outside of function
    deriving (Show)
makePrisms ''ExecInterrupt

instance IsString ExecInterrupt where
    fromString = Error . fromString

prefixError :: MonadError ExecInterrupt m => Text -> m a -> m a
prefixError desc = mapError (_Error %~ (desc <>))

type ULabelCoords = M.Map UserLabelId StmtFunCoord

data ExecEnv = ExecEnv
    { _evFunDecls     :: FunDecls
    , _evULabelCoords :: ULabelCoords
    , _evCurFun       :: FunName
    }

makeLenses ''ExecEnv

type MonadExec m =
    ( MonadIO m
    , MonadError ExecInterrupt m
    , MonadState LocalVars m
    , MonadReader ExecEnv m
    , MonadRefEnv ExpRes m
    )

-- | Adds current statement info to probable evaluation error
withStmt :: MonadError ExecInterrupt m => Stmt -> m a -> m a
withStmt stmt =
    flip catchError $
    throwError . (_Error %~ sformat (shown%": "%stext) stmt)

