{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Toy.Lang.Data where

import           Control.Lens              (makePrisms)
import           Control.Monad.Error.Class (MonadError (..))
import           Control.Monad.Reader      (MonadReader)
import           Control.Monad.State       (MonadState)
import           Control.Monad.Trans       (MonadIO)
import qualified Data.Map                  as M
import           Data.Monoid               ((<>))
import           Data.String               (IsString (..))
import           Formatting                (sformat, sformat, shown, stext, (%))
import           GHC.Exts                  (IsList (..))
import           Universum                 hiding (toList)

import           Toy.Base                  (FunSign (..), Var (..))
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

type FunDecls = M.Map Var FunDecl

-- | Builds function declarations map
mkFunDecls :: (IsList l, Item l ~ FunDecl) => l -> FunDecls
mkFunDecls = fromList . map doLol . toList
  where
    doLol d@(FunSign n _, _) = (n, d)

data Program = Program
    { pFunDecls :: FunDecls
    , pMain     :: Stmt
    } deriving (Show)

toProgram :: Stmt -> Program
toProgram = Program mempty

data ExecInterrupt
    = Error Text       -- ^ Execution exception
    | Returned ExpRes  -- ^ Function returns
    deriving (Show)
makePrisms ''ExecInterrupt

instance IsString ExecInterrupt where
    fromString = Error . fromString

prefixError :: MonadError ExecInterrupt m => Text -> m a -> m a
prefixError desc = mapError (_Error %~ (desc <>))

type MonadExec m =
    ( MonadIO m
    , MonadError ExecInterrupt m
    , MonadState LocalVars m
    , MonadReader FunDecls m
    , MonadRefEnv ExpRes m
    )

-- | Adds current statement info to probable evaluation error
withStmt :: MonadError ExecInterrupt m => Stmt -> m a -> m a
withStmt stmt =
    flip catchError $
    throwError . (_Error %~ sformat (shown%": "%stext) stmt)


