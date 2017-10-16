module Network.Wai.Handler.Node.Utils
  ( withAVar
  , nextTick
  , nextTickEff
  ) where

import Prelude

import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Aff.AVar (AVAR, AVar, takeVar, putVar)
import Control.Monad.Eff (Eff)
import Control.Monad.Error.Class (throwError, catchError)

import Data.Either (Either(Right))
import Data.Monoid (mempty)

import Unsafe.Coerce (unsafeCoerce)

withAVar :: forall e a b. AVar a -> (a -> Aff (avar :: AVAR | e) b) -> Aff (avar :: AVAR | e) b
withAVar v aff = do
  a <- takeVar v
  b <- aff a `catchError` \e -> putVar a v *> throwError e
  putVar a v *> pure b

nextTick :: forall eff. Aff eff Unit
nextTick = makeAff \cb -> nextTickEff (cb (Right unit)) *> pure mempty

mkEff :: forall eff a. (Unit -> a) -> Eff eff a
mkEff = unsafeCoerce

nextTickEff :: forall eff. Eff eff Unit -> Eff eff Unit
nextTickEff callback = mkEff \_ -> _process.nextTick callback

foreign import _process :: forall props. { | props }
