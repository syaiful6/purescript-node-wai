module Examples.HelloWorld where

import Prelude

import Control.Monad.Aff (launchAff)
import Control.Monad.Eff (Eff)

import Data.Maybe (Maybe(Nothing))
import Data.Tuple (Tuple(..))
import Data.List (List(Nil), (:))
import Data.ByteString.Builder (string7)

import Network.Wai (Application, responseBuilder)
import Network.Wai.Types (status200, hContentType)
import Network.Wai.Handler.Node.Run (runDefault)
import Network.Wai.Handler.Node.Effects (WaiEffects)
import Network.Wai.Handler.Node.Types (SocketOption(SockTCP))

app :: forall eff. Application eff
app _ respond = respond $
  responseBuilder status200 (Tuple hContentType "text/plain" : Nil) (string7 "Hello World")

main :: forall eff. Eff (WaiEffects eff) Unit
main = void $ launchAff $ runDefault (SockTCP "localhost" 2048 Nothing) app
