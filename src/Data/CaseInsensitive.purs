module Data.CaseInsensitive
  ( CI
  , mkCI
  , foldedCase
  , originalCase
  , class FoldCase
  , foldCase
  ) where

import Prelude

import Control.Monad.Eff (forE)
import Control.Monad.Eff.Unsafe (unsafePerformEff)

import Data.ByteString as B
import Data.ByteString.Internal (create, peekByteOff, pokeByteOff, plusPtr)
import Data.String (toLower) as S
import Data.Maybe (maybe)
import Data.Monoid (class Monoid, mempty)

data CI s = CI s s

mkCI :: forall s. FoldCase s => s -> CI s
mkCI s = CI s (foldCase s)

foldedCase :: forall s. CI s -> s
foldedCase (CI _ s) = s

originalCase :: forall s. CI s -> s
originalCase (CI t _) = t

instance eqCI :: Eq s => Eq (CI s) where
  eq (CI _ s) (CI _ t) = s == t

instance ordCI :: Ord s => Ord (CI s) where
  compare (CI _ s) (CI _ t) = s `compare` t

instance semigroupCI :: Semigroup s => Semigroup (CI s) where
  append (CI o f) (CI o2 f2) = CI (o <> o2) (f <> f2)

instance monoidCI :: Monoid s => Monoid (CI s) where
  mempty = CI mempty mempty

class FoldCase s where
  foldCase :: s -> s

-- | Note that @foldCase@ on @'B.ByteString's@ is only guaranteed to be correct for ISO-8859-1 encoded string
instance byteStringFoldCase :: FoldCase B.ByteString where
  foldCase (B.ByteString p0 s len) = unsafePerformEff $ create len \p2 ->
    let p1 = p0 `plusPtr` s
    in forE s len \n -> do
      ms <- peekByteOff p1 n
      maybe (pure unit) (pokeByteOff p2 n <<< toLower8) ms

instance stringFoldCase :: FoldCase String where
  foldCase = S.toLower

toLower8 :: Int -> Int
toLower8 w
  |  65 <= w && w <=  90 ||
    192 <= w && w <= 214 ||
    216 <= w && w <= 222 = w + 32
  | otherwise            = w
