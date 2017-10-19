module Network.Wai.Handler.Node.Utils
  ( withAVar
  , nextTick
  , nextTickEff
  , writeRawHTTP
  , endRawHTTP
  , getDateCurrent
  ) where

import Prelude

import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Aff.AVar (AVAR, AVar, takeVar, putVar)
import Control.Monad.Eff (Eff)
import Control.Monad.Error.Class (throwError, catchError)
import Control.Monad.Eff.Exception (Error)

import Data.Either (Either(Right))
import Data.Monoid (mempty)
import Data.Function.Uncurried as Fn
import Data.JSDate (JSDate)

import Node.HTTP (Response)
import Node.Buffer (Buffer)

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

writeRawHTTP :: forall eff. Response -> Buffer -> Aff eff Unit
writeRawHTTP res buf = makeAff \cb -> do
  _ <- Fn.runFn4 _writeRawHTTP Right res buf cb
  pure mempty

endRawHTTP :: forall eff. Response -> Aff eff Unit
endRawHTTP res = makeAff \cb -> do
  _ <- Fn.runFn3 _endRawHTTP Right res cb
  pure mempty

foreign import _process :: forall props. { | props }

foreign import _writeRawHTTP
  :: forall eff
   . Fn.Fn4
      (forall x y. y -> Either x y)
      Response
      (Buffer)
      (Either Error Unit -> Eff eff Unit)
      (Eff eff Unit)

foreign import _endRawHTTP
  :: forall eff
   . Fn.Fn3
      (forall x y. y -> Either x y)
      Response
      (Either Error Unit -> Eff eff Unit)
      (Eff eff Unit)

foreign import getDateCurrent :: forall eff. Eff eff JSDate
