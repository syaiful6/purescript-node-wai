module Network.HTTP.Wai.Run
  ( handleRequest
  , module Exports
  ) where

import Prelude

import Control.Monad.Aff (Aff, runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (throwException)

import Node.HTTP as NH

import Network.HTTP.Wai (Application, toWaiRequest, reqHeaders)
import Network.HTTP.Wai (Application, Middleware) as Exports
import Network.HTTP.Wai.Effects (WaiEffects)
import Network.HTTP.Wai.Header (keyedRequestHeader)
import Network.HTTP.Wai.Response (sendResponse)
import Network.HTTP.Wai.Internal (ResponseReceived(..))

handleRequest
  :: forall eff
   . Application (WaiEffects eff)
  -> NH.Request
  -> NH.Response
  -> Eff (WaiEffects eff) Unit
handleRequest app req res = do
  let wreq   = toWaiRequest req
      hmap   = keyedRequestHeader $ reqHeaders wreq
      sender wres = do
        handleAff (sendResponse res wreq hmap wres)
        pure ResponseReceived
  _ <- app wreq sender
  pure unit

handleAff
  :: forall eff a
   . Aff (WaiEffects eff) a
  -> Eff (WaiEffects eff) Unit
handleAff = void <<< runAff throwException (const (pure unit))
