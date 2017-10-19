module Network.Wai.Handler.Node.Date
  ( GMTDate
  , withDateCache
  ) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Schedule.AutoUpdate (UpdateSettings(..), mkAutoUpdate)
import Control.Monad.Aff.Schedule.Effects (ScheduleEff)

import Data.ByteString (pack, ByteString)
import Data.String (toCharArray)
import Data.Char (toCharCode)
import Data.Int.Bits ((.&.))
import Data.JSDate (JSDate, toUTCString)
import Data.Newtype (wrap)

import Network.Wai.Handler.Node.Utils (getDateCurrent)

-- | The type of the Date header value.
type GMTDate = ByteString

withDateCache
  :: forall eff
   . (Aff (ScheduleEff eff) GMTDate -> Aff (ScheduleEff eff) a)
  -> Aff (ScheduleEff eff) a
withDateCache action = initialize >>= action

initialize
  :: forall eff
   . Aff (ScheduleEff eff) (Aff (ScheduleEff eff) GMTDate)
initialize = mkAutoUpdate $ UpdateSettings (wrap 1000) (str2Bs <$> getCurrentHTTPDate)

getCurrentHTTPDate :: Aff eff JSDate
getCurrentHTTPDate = liftEff getDateCurrent

str2Bs :: String -> ByteString
str2Bs = pack <<< map (\c -> toCharCode c .&. 0xFF) <<< toCharArray
