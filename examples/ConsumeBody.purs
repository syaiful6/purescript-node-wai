module Examples.ConsumeBody where

import Prelude

import Control.Monad.Aff (Aff, launchAff, delay)
import Control.Monad.Eff (Eff)

import Data.Maybe (Maybe(Nothing))
import Data.Tuple (Tuple(..))
import Data.List (List(Nil), (:))
import Data.ByteString as S
import Data.ByteString.Builder (Builder, string7, byteString)
import Data.Newtype (wrap)

import Network.Wai (Application, Request(..), responseStream)
import Network.Wai.Types (status200, hContentType)
import Network.Wai.Handler.Node.Run (runDefault)
import Network.Wai.Handler.Node.Effects (WaiEffects)
import Network.Wai.Handler.Node.Types (SocketOption(SockTCP))

printBody
  :: forall eff
   . (Builder -> Aff eff Unit)
  -> Aff eff S.ByteString
  -> Aff eff Unit
printBody send = go
  where
  go recv = do
    s <- recv
    unless (S.null s) do
      send (byteString s)
      go recv


app :: forall eff. Application eff
app (Request { body }) respond = respond $ responseStream status200 (Tuple hContentType "text/plain" : Nil) $
  \send flush -> do
    printBody send body
    _ <- delay (wrap 1000.00) -- delay one second
    flush
    send (string7 "All done!\n")

main :: forall eff. Eff (WaiEffects eff) Unit
main = void $ launchAff $ runDefault (SockTCP "localhost" 2048 Nothing) app
