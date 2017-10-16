module Network.Wai.Handler.Node.Timeout
  ( Manager
  , TimeoutAction
  , Handle
  , initialize
  , stopManager
  , killManager
  , withManager
  , register
  , tickle
  , cancel
  , pause
  , resume
  ) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Schedule.Reaper
  (Reaper, ReaperSetting(..), mkReaper, reaperAdd, reaperStop, reaperKill)
import Control.Monad.Error.Class (catchError)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (REF, Ref, newRef, writeRef, readRef)

import Data.CatList (CatList, empty, cons, null, snoc)
import Data.Foldable (traverse_, foldl)
import Data.Traversable (traverse)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)

import Network.Wai.Handler.Node.Effects (WaiEffects)


type Manager eff = Reaper eff (CatList (Handle eff)) (Handle eff)

type TimeoutAction eff = Aff eff Unit

data Handle eff = Handle (Ref (TimeoutAction eff)) (Ref State)

data State = Active    -- Manager turns it to Inactive.
           | Inactive  -- Manager removes it with timeout action.
           | Paused    -- Manager does not change it.
           | Canceled  -- Manager removes it without timeout action.

initialize :: forall eff. Number -> Aff (WaiEffects eff) (Manager (WaiEffects eff))
initialize timeout = mkReaper $ ReaperSetting
  { action: mkCatListAction prune
  , delay: wrap timeout
  , cons: cons
  , isNull: null
  , empty: empty
  }
  where
  prune m@(Handle act st) = do
    state <- liftEff $ do
      x <- readRef st
      writeRef st (inactivate x)
      pure x
    case state of
      Inactive -> do
        onTimeout <- liftEff (readRef act)
        _ <- onTimeout `catchError` \_ -> pure unit
        pure Nothing
      Canceled -> pure Nothing
      _        -> pure $ Just m

  inactivate Active = Inactive
  inactivate x = x

stopManager :: forall eff. Manager (ref :: REF | eff) -> Aff (ref :: REF | eff) Unit
stopManager rep = reaperStop rep >>= traverse_ fire
  where
  fire (Handle act _) = do
    onTimeout <- liftEff (readRef act)
    onTimeout `catchError` \_ -> pure unit

killManager :: forall eff. Manager eff -> Aff eff Unit
killManager = reaperKill

register
  :: forall eff
   . Manager (ref :: REF | eff)
  -> TimeoutAction (ref :: REF | eff)
  -> Aff (ref :: REF | eff) (Handle (ref :: REF | eff))
register mgr onTimeout = do
  act   <- liftEff $ newRef onTimeout
  state <- liftEff $ newRef Active
  let h = Handle act state
  _ <- reaperAdd mgr h
  pure h

tickle :: forall eff. Handle (ref :: REF | eff) -> Aff (ref :: REF | eff) Unit
tickle (Handle _ st) = liftEff $ writeRef st Active

cancel :: forall eff. Handle (ref :: REF | eff) -> Aff (ref :: REF | eff) Unit
cancel (Handle act st) = liftEff do
  _ <- writeRef act (pure unit)
  writeRef st Canceled

pause :: forall eff. Handle (ref :: REF | eff) -> Aff (ref :: REF | eff) Unit
pause (Handle _ st) = liftEff $ writeRef st Paused

resume :: forall eff. Handle (ref :: REF | eff) -> Aff (ref :: REF | eff) Unit
resume = tickle

withManager :: forall eff a. Number -> (Manager (WaiEffects eff) -> Aff (WaiEffects eff) a) -> Aff (WaiEffects eff) a
withManager timeout f = initialize timeout >>= f

mkCatListAction
  :: forall eff item item'
   . (item -> Aff eff (Maybe item'))
  -> CatList item
  -> Aff eff (CatList item' -> CatList item')
mkCatListAction f old = do
  new <- map (mapMaybeM id) $ traverse f old
  pure $ append new

mapMaybeM :: forall a a'. (a -> Maybe a') -> CatList a -> CatList a'
mapMaybeM p xs = foldl select empty xs
  where
  select cq i = case p i of
    Nothing -> cq
    Just x  -> snoc cq x
