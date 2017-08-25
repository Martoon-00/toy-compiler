-- | Lang program processor

module Toy.Lang.Util where

import           Universum

import qualified Data.Set      as S
import           Toy.Exp       (UserLabelId)
import           Toy.Lang.Data (Stmt (..))
import           Toy.Util      ((<?>))

gatherULabels :: Stmt -> Either Text (S.Set UserLabelId)
gatherULabels = \case
    Label lid -> pure $ one lid
    Seq s1 s2 -> do
        [r1, r2] <- mapM gatherULabels [s1, s2]
        maybe (Left "Duplicated labels!") Right (r1 <?> r2)
    _         -> pure mempty
