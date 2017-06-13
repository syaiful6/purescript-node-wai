module Test.Main where

import Prelude

import Control.Monad.Aff.AVar (AVAR)

import Test.QuickCheck (QC)
import Test.Unit (suite)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)

main :: forall eff. QC (testOutput :: TESTOUTPUT, avar :: AVAR | eff) Unit
main = runTest $ suite "Node Wai" do
  pure unit
