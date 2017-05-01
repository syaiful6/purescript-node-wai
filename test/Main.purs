module Test.Main where

import Prelude

import Control.Monad.Aff.AVar (AVAR)

import Data.Maybe (isJust, isNothing, fromMaybe)

import Test.QuickCheck (QC)
import Test.Unit (suite, test)
import Test.Unit.Assert (assert)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)

import Network.HTTP.Types.Header as HD


testHeadersData :: HD.RequestHeaders
testHeadersData =
  [ HD.contentType "text/json"
  , HD.accept "text/json"
  , HD.acceptEncoding "gzip, deflate, sdch"
  , HD.userAgent "Mozilla/5.0"
  ]

main :: forall eff. QC (testOutput :: TESTOUTPUT, avar :: AVAR | eff) Unit
main = runTest $ suite "Bookie" do
  suite "Network.HTTP.Types.Header" do
    test "case insensivetive" do
      let ctype = HD.ci2Head "content-type"
      assert "empty value" $ ctype == (HD.ContentType)
    test "lookup header" do
      let ctype = HD.lookupHeader HD.ContentType testHeadersData
      assert "lookup" $ isJust ctype
      assert "lookupHeader" $ fromMaybe false (map ((==) "text/json") ctype)
