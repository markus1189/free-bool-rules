{-# LANGUAGE LambdaCase #-}
module FreeBool.Transformation ( rewrite
                               , subterms
                               , pures
                               , nnf
                               , transformNorToAnd
                               , transformNandToOr
                               , transformRemoveNotNot
                               , optimizeAnd
                               , optimizeOr
                               , optimizeNot
                               ) where

import Data.Foldable (foldl')
import Control.Lens.Traversal (Traversal')
import FreeBool.Types
import Control.Lens.Plated (rewriteOf)
import Control.Applicative ((<|>), liftA2, empty)

rewrite :: [FreeBool a -> Maybe (FreeBool a)] -> FreeBool a -> FreeBool a
rewrite = rewriteOf subterms . asum'
  where asum' = foldl' (liftA2 (<|>)) (const empty)

subterms :: Traversal' (FreeBool a) (FreeBool a)
subterms f = \case
  BAnd lhs rhs -> BAnd <$> f lhs <*> f rhs
  BOr lhs rhs -> BOr <$> f lhs <*> f rhs
  BNot fa -> BNot <$> f fa
  BFalse -> pure BFalse
  BTrue -> pure BTrue
  Pure x -> pure (Pure x)

pures :: Traversal' (FreeBool a) a
pures f = \case
  Pure x -> Pure <$> f x
  x -> pure x

nnf :: FreeBool a -> FreeBool a
nnf = rewrite [transformNorToAnd, transformNandToOr, transformRemoveNotNot]

transformNorToAnd (BNot (BOr x y)) = Just $ BAnd (BNot x) (BNot y)
transformNorToAnd _ = Nothing

transformNandToOr (BNot (BAnd x y)) = Just $ BOr (BNot x) (BNot y)
transformNandToOr _ = Nothing

transformRemoveNotNot (BNot (BNot x)) = Just x
transformRemoveNotNot _ = Nothing

optimizeAnd :: FreeBool a -> Maybe (FreeBool a)
optimizeAnd = \case
  (BAnd BFalse _) -> Just BFalse
  (BAnd _ BFalse) -> Just BFalse
  (BAnd BTrue x) -> Just x
  (BAnd x BTrue) -> Just x
  _ -> Nothing

optimizeOr :: FreeBool a -> Maybe (FreeBool a)
optimizeOr = \case
  (BOr BTrue _) -> Just BTrue
  (BOr _ BTrue) -> Just BTrue
  (BOr BFalse x) -> Just x
  (BOr x BFalse) -> Just x
  _ -> Nothing

optimizeNot :: FreeBool a -> Maybe (FreeBool a)
optimizeNot = \case
  BNot BTrue -> Just BFalse
  BNot BFalse -> Just BTrue
  BNot (BNot x) -> Just x
  _ -> Nothing
