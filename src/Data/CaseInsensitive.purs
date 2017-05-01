module Data.CaseInsensitive
  ( class FoldCase
  , foldCase
  , CaseInsensitive
  , CI
  , mkCI
  ) where

import Prelude

import Data.Function (on)
import Data.String as S
import Data.Monoid (class Monoid, mempty)

data CaseInsensitive s = CaseInsensitive s s

-- type alias for string
type CI = CaseInsensitive String

instance eqCaseInsensitive :: Eq a => Eq (CaseInsensitive a) where
  eq = (==) `on` foldedCase

instance ordCaseInsensitive :: Ord a => Ord (CaseInsensitive a) where
  compare = compare `on` foldedCase

instance semigroupCaseInsensitive :: Semigroup a => Semigroup (CaseInsensitive a) where
  append (CaseInsensitive a1 b1) (CaseInsensitive a2 b2) = CaseInsensitive (a1 <> a2) (b1 <> b2)

instance monoidCaseInsensitive :: Monoid a => Monoid (CaseInsensitive a) where
  mempty = CaseInsensitive mempty mempty

mkCI :: forall s. FoldCase s => s -> CaseInsensitive s
mkCI s = CaseInsensitive s (foldCase s)

foldedCase :: forall s. CaseInsensitive s -> s
foldedCase (CaseInsensitive _ s) = s

original :: forall s. CaseInsensitive s -> s
original (CaseInsensitive s _) = s

class FoldCase s where
  foldCase :: s -> s

instance stringFoldCase :: FoldCase String where
  foldCase = S.toLower
