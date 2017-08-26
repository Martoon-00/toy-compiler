-- | Lang program processor

module Toy.Lang.Util where

import           Universum

import qualified Data.Map      as M
import qualified Data.Set      as S

import           Toy.Exp       (UserLabelId)
import           Toy.Lang.Data (Branch (..), Program (..), Stmt (..), StmtCoord,
                                StmtFunCoord (..), ULabelCoords)
import           Toy.Util      (foldMapNoDups, (<?>))

buildULabelsMap :: Program -> Either Text ULabelCoords
buildULabelsMap Program{..} = do
    let ulmPerFun = M.toList getProgram <&> \(funName, (_, stmt)) ->
            StmtFunCoord funName <<$>> findStmtLabels stmt
    toEither $ foldMapNoDups one $ mconcat ulmPerFun
  where
    findStmtLabels :: Stmt -> [(UserLabelId, StmtCoord)]
    findStmtLabels = \case
        If _ s1 s2    -> branch s1 s2
        Seq s1 s2     -> branch s1 s2
        DoWhile s _   -> findStmtLabels s
        Label l       -> one (l, mempty)

        _ := _        -> mempty
        Return _      -> mempty
        ArrayAssign{} -> mempty
        Skip          -> mempty
        Goto _        -> mempty

    branch s1 s2 = do
        let l = findStmtLabels s1
        let r = findStmtLabels s2
        ((LeftPath :) <<$>> l) <> ((RightPath :) <<$>> r)

    toEither = maybe (Left "Duplicated user labels!") Right

gatherULabels :: Stmt -> Either Text (S.Set UserLabelId)
gatherULabels = \case
    Label lid -> pure $ one lid
    Seq s1 s2 -> do
        [r1, r2] <- mapM gatherULabels [s1, s2]
        maybe (Left "Duplicated labels!") Right (r1 <?> r2)
    _         -> pure mempty

balanceStmt :: Stmt -> Stmt
balanceStmt = identity  -- TODO

balanceProgram :: Program -> Program
balanceProgram (Program p) = Program (balanceStmt <<$>> p)

