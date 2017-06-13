module Network.Wai.Handler.Node.Timeout where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Schedule.Reaper
  (Reaper, ReaperSetting(..), mkReaper, mkListAction, reaperAdd, reaperStop, reaperKill)
import Control.Monad.Error.Class (catchError)
import Control.Monad.STM (TVar, newTVarAff, writeTVar, readTVar, atomically)

import Data.List (List(Nil), (:), null)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)

import Network.Wai.Handler.Node.Effects (WaiEffects)


type Manager eff = Reaper (WaiEffects eff) (List (Handle eff)) (Handle eff)

type TimeoutAction eff = Aff (WaiEffects eff) Unit

data Handle eff = Handle (TVar (TimeoutAction eff)) (TVar State)

data State = Active    -- Manager turns it to Inactive.
           | Inactive  -- Manager removes it with timeout action.
           | Paused    -- Manager does not change it.
           | Canceled  -- Manager removes it without timeout action.

initialize :: forall eff. Number -> Aff (WaiEffects eff) (Manager eff)
initialize timeout = mkReaper $ ReaperSetting
  { action: mkListAction prune
  , delay: wrap timeout
  , cons: (:)
  , isNull: null
  , empty: Nil
  }
  where
  prune m@(Handle act st) = do
    state <- atomically $ do
      x <- readTVar st
      writeTVar st (inactivate x)
      pure x
    case state of
      Inactive -> do
        onTimeout <- atomically (readTVar act)
        _ <- onTimeout `catchError` \_ -> pure unit
        pure Nothing
      Canceled -> pure Nothing
      _        -> pure $ Just m

  inactivate Active = Inactive
  inactivate x = x

stopManager :: forall eff. Manager eff -> Aff (WaiEffects eff) Unit
stopManager rep = reaperStop rep >>= traverse_ fire
  where
  fire (Handle act _) = do
    onTimeout <- atomically (readTVar act)
    onTimeout `catchError` \_ -> pure unit

killManager :: forall eff. Manager eff -> Aff (WaiEffects eff) Unit
killManager = reaperKill

register :: forall eff. Manager eff -> TimeoutAction eff -> Aff (WaiEffects eff) (Handle eff)
register mgr onTimeout = do
  act   <- newTVarAff onTimeout
  state <- newTVarAff Active
  let h = Handle act state
  _ <- reaperAdd mgr h
  pure h

tickle :: forall eff. Handle eff -> Aff (WaiEffects eff) Unit
tickle (Handle _ st) = atomically $ writeTVar st Active
