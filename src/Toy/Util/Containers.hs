-- | Utils to work with containers

module Toy.Util.Containers where

import qualified Data.Set  as S
import           Universum

infixr 6 <?>
(<?>) :: Ord a => S.Set a -> S.Set a -> Maybe (S.Set a)
s1 <?> s2 = do
    let r = s1 <> s2
    guard (S.size s1 + S.size s2 == S.size r)
    return r

foldMapNoDups :: Ord b => (a -> S.Set b) -> [a] -> Maybe (S.Set b)
foldMapNoDups _ []     = Just mempty
foldMapNoDups f (x:xs) = do
    ys <- foldMapNoDups f xs
    f x <?> ys
