-- | Utils to work with containers

module Toy.Util.Containers where

import           Universum

infixr 6 <?>
(<?>) :: (NontrivialContainer t, Monoid t) => t -> t -> Maybe t
s1 <?> s2 = do
    let r = s1 <> s2
    guard (length s1 + length s2 == length r)
    return r

foldMapNoDups
    :: (NontrivialContainer t, Monoid t)
    => (a -> t) -> [a] -> Maybe t
foldMapNoDups _ []     = Just mempty
foldMapNoDups f (x:xs) = do
    ys <- foldMapNoDups f xs
    f x <?> ys
