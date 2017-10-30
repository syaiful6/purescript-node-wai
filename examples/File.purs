module Examples.File where

import Prelude

import Control.Monad.Aff (launchAff)
import Control.Monad.Eff (Eff)

import Data.Maybe (Maybe(Nothing))
import Data.Tuple (Tuple(..))
import Data.List (List(Nil), (:))

import Network.Wai (Application, responseFile)
import Network.Wai.Types (status200, hContentType, hContentLength)
import Network.Wai.Handler.Node.Run (runDefault)
import Network.Wai.Handler.Node.Effects (WaiEffects)
import Network.Wai.Handler.Node.Types (SocketOption(SockTCP))

app :: forall eff. Application eff
app _ respond = respond $ responseFile status200
                  (Tuple hContentType "application/json": Tuple hContentLength "1638" : Nil)
                  "bower.json"
                  Nothing

main :: forall eff. Eff (WaiEffects eff) Unit
main = void $ launchAff $ runDefault (SockTCP "localhost" 2048 Nothing) app
