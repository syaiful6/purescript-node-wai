module Network.Wai.Handler.Node.Utils
  ( withAVar
  , handleAff
  ) where

import Prelude

import Control.Monad.Aff (Aff, runAff)
import Control.Monad.Aff.AVar (AVAR, AVar, takeVar, putVar)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION, throwException)
import Control.Monad.Error.Class (throwError, catchError)

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
