module Network.Wai.Handler.Node.Utils
  ( withAVar
  , handleAff
  , nextTick
  , nextTickEff
  ) where

import Prelude

import Control.Monad.Aff (Aff, runAff, makeAff)
import Control.Monad.Aff.AVar (AVAR, AVar, takeVar, putVar)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION, throwException)
import Control.Monad.Error.Class (throwError, catchError)

import Unsafe.Coerce (unsafeCoerce)

withAVar :: forall e a b. AVar a -> (a -> Aff (avar :: AVAR | e) b) -> Aff (avar :: AVAR | e) b
withAVar v aff = do
  a <- takeVar v
  b <- aff a `catchError` \e -> putVar v a *> throwError e
  putVar v a *> pure b

handleAff
  :: forall eff a
   . Aff (exception :: EXCEPTION | eff) a
  -> Eff (exception :: EXCEPTION | eff) Unit
handleAff = void <<< runAff throwException (const (pure unit))

nextTick :: forall eff. Aff eff Unit
nextTick = makeAff \_ succ -> nextTickEff (succ unit)

mkEff :: forall eff a. (Unit -> a) -> Eff eff a
mkEff = unsafeCoerce

nextTickEff :: forall eff. Eff eff Unit -> Eff eff Unit
nextTickEff callback = mkEff \_ -> _process.nextTick callback

foreign import _process :: forall props. { | props }
