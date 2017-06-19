module Test.Network.Wai.Handler.Node.BodySourceSpec
  ( mainBodySourceSpec
  ) where

import Prelude

import Control.Monad.Aff (delay)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Random (RANDOM)

import Data.ByteString as B
import Data.Newtype (wrap)

import Test.Unit (TestSuite, describe, it)
import Test.Unit.Assert (shouldEqual)

import Network.Wai.Handler.Node.Effects (WaiEffects)
import Network.Wai.Handler.Node.BodySource (readBody, mkBodySource')

import Node.FS.Stream (createReadStream)

type TestEff eff = WaiEffects (random :: RANDOM | eff)

goDrain recv = do
  buf <- recv
  let len = B.length buf
  liftEff (log (show len))
  _ <- delay (wrap 5.00)
  if len <= 0 then pure unit else goDrain recv

mainBodySourceSpec :: forall eff. TestSuite (TestEff eff)
mainBodySourceSpec = do
  describe "Body Source" do
    it "Can read big file" do
      -- this stream has high watermark 64kb
      rstream <- liftEff $ createReadStream "./bigfile.mp4"
      -- set to 64kb
      bso <- mkBodySource' (64 * 1024)
      recv0 <- readBody rstream bso
      goDrain recv0
