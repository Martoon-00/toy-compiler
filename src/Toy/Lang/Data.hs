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
                                            UserLabelId, readE, (==:))
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


-- TODO: move to separate module and remove suffix @s@

-- | Drops diven expression
dropS :: Exp -> Stmt
dropS e = "_" := e

-- | @while@ loop in terms of `Stmt`.
whileS :: Exp -> Stmt -> Stmt
whileS cond stmt = If cond (DoWhile stmt cond) Skip

-- | @repeat@ loop in terms of `Stmt`.
repeatS :: Stmt -> Exp -> Stmt
repeatS stmt stop = DoWhile stmt (stop ==: 0)

-- | @repeat@ loop in terms of `Stmt`.
forS :: Stmt -> Exp -> Stmt -> Stmt -> Stmt
forS s1 cond sr body = s1 <> whileS cond (body <> sr)

-- | @read@ to a given variable.
readS :: Var -> Stmt
readS v = v := readE

-- | Function call in terms of `Stmt`.
funCallS :: Var -> [Exp] -> Stmt
funCallS name args = dropS $ FunE name args

-- | @write@ given expression.
writeS :: Exp -> Stmt
writeS = funCallS "write" . pure

-- | Array initializer, which imideatelly writes to variable.
arrayVarS :: Var -> [Exp] -> Stmt
arrayVarS var exps = mconcat
    [ var := ArrayUninitE (length exps)
    , uncurry (ArrayAssign $ VarE var) `foldMap` (zip (map ValueE [0..]) exps)
    ]

-- | Array initializer, which allows to get array as exression.
arrayS :: (Exp -> Stmt) -> [Exp] -> Stmt
arrayS f exps = mconcat
    [ arrayVarS "_" exps
    , f "_"
    ]

-- | Goto given label by name.
gotoS :: UserLabelId -> Stmt
gotoS = Goto . LabelE
