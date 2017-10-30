module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM)

import Test.Unit (suite)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)

import Node.Buffer (BUFFER)

import Network.Wai.Handler.Node.Effects (WaiEffects)
import Test.Network.Wai.Handler.Node.ResponseHeader (mainResponseHeaderSpec)
import Test.Network.Wai.Handler.Node.FdCacheSpec (fdSpecMain)

main :: forall eff. Eff (WaiEffects (random :: RANDOM, testOutput :: TESTOUTPUT, buffer :: BUFFER | eff)) Unit
main = runTest $ suite "Node Wai" do
  mainResponseHeaderSpec
  fdSpecMain
