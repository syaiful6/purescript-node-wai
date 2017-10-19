module Network.Wai.Handler.Node.Date
  ( GMTDate
  , withDateCache
  ) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Schedule.AutoUpdate (UpdateSettings(..), mkAutoUpdate)
import Control.Monad.Aff.Schedule.Effects (ScheduleEff)
import Control.Monad.Eff.Class (liftEff)

import Data.JSDate (JSDate, toUTCString)
import Data.Newtype (wrap)

import Network.Wai.Handler.Node.Utils (getDateCurrent)

-- | The type of the Date header value.
type GMTDate = String

withDateCache
  :: forall eff a
   . (Aff (ScheduleEff eff) GMTDate -> Aff (ScheduleEff eff) a)
  -> Aff (ScheduleEff eff) a
withDateCache action = initialize >>= action

initialize
  :: forall eff
   . Aff (ScheduleEff eff) (Aff (ScheduleEff eff) GMTDate)
initialize = mkAutoUpdate $ UpdateSettings (wrap 1000.00) (toUTCString <$> getCurrentHTTPDate)

getCurrentHTTPDate :: forall eff. Aff eff JSDate
getCurrentHTTPDate = liftEff getDateCurrent