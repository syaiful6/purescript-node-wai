module Test.Network.Wai.Handler.Node.FdCacheSpec (fdSpecMain) where

import Prelude

import Control.Monad.Aff (throwError, error)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (newRef, writeRef, readRef)

import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))

import Test.Unit (TestSuite, describe, it)
import Test.Unit.Assert (expectFailure)

import Unsafe.Coerce (unsafeCoerce)

import Node.FS.Aff as F
import Node.Buffer as Buffer

import Network.Wai.Handler.Node.FdCache (withFdCache, Fd)
import Network.Wai.Handler.Node.Effects (WaiEffects)

fakeFd :: Fd
fakeFd = unsafeCoerce (-1)

fdSpecMain :: forall eff. TestSuite (WaiEffects (buffer :: Buffer.BUFFER | eff))
fdSpecMain = describe "withFdCache" do
  it "cleanup" do
    ref <- liftEff (newRef fakeFd)
    buf <- liftEff (Buffer.create 100)
    _ <- withFdCache 60.00 \getFd -> do
      Tuple mfd _ <- getFd "bower.json"
      case mfd of
        Just fd -> liftEff (writeRef ref fd)
        Nothing -> throwError (error "cant get fd")
    nfd <- liftEff (readRef ref)
    expectFailure "should be invalid fd" (void $ F.fdRead nfd buf 0 100 Nothing)
