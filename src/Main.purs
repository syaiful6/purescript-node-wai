module Main where

import Prelude

import Data.Maybe (Maybe(Nothing))

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Node.HTTP (createServer, listen)
import Network.HTTP.Wai.Effects (WaiEffects)
import Network.HTTP.Wai as H
import Network.HTTP.Wai.Run (handleRequest, Application)

type MainEffect eff = WaiEffects (console :: CONSOLE | eff)


simpleApp :: forall eff. Application (MainEffect eff)
simpleApp _ respond = do
  log $ "Receive request"
  respond $ H.responseStrUtf8 H.status200 [H.contentType "text/plain"] "Hi guys"

main :: forall eff. Eff (MainEffect eff) Unit
main = do
  server <- createServer (handleRequest simpleApp)
  listen server { hostname: "localhost", port: 2123, backlog: Nothing } $ do
    log "Listening on port 2123."
