module Examples.StreamingResponse where

import Prelude

import Control.Monad.Aff (launchAff, delay)
import Control.Monad.Eff (Eff)

import Data.Maybe (Maybe(Nothing))
import Data.Tuple (Tuple(..))
import Data.List (List(Nil), (:))
import Data.ByteString.Builder (string7)
import Data.Newtype (wrap)

import Network.Wai (Application, responseStream)
import Network.Wai.Types (status200, hContentType)
import Network.Wai.Handler.Node.Run (runDefault)
import Network.Wai.Handler.Node.Effects (WaiEffects)
import Network.Wai.Handler.Node.Types (SocketOption(SockTCP))

app :: forall eff. Application eff
app _ respond = respond $ responseStream status200 (Tuple hContentType "text/plain" : Nil) $
  \send flush -> do
    _ <- send (string7 "Starting the response...\n")
    _ <- delay (wrap 1000.00) -- delay one second
    flush
    send (string7 "All done!\n")

main :: forall eff. Eff (WaiEffects eff) Unit
main = void $ launchAff $ runDefault (SockTCP "localhost" 2048 Nothing) app
