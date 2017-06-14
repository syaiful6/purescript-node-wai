module Network.Wai.Handler.Node.FdCache
  ( withFdCache
  , Fd
  , Refresh
  , openFile
  , closeFile
  ) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Schedule.Reaper
  (Reaper, mkReaper, ReaperSetting(..), reaperRead, reaperAdd, reaperStop)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Error.Class (withResource)
import Control.Monad.Eff.Ref (REF, Ref, newRef, writeRef, readRef)

import Data.Foldable (traverse_, foldr)
import Data.List (List)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Tuple (Tuple(..), uncurry)
import Data.Traversable (traverse)

import Node.FS.Aff as F
import Node.FS (FS, FileDescriptor, FileFlags(..))
import Node.Path (FilePath)
import Network.Wai.Handler.Node.Effects (WaiEffects)


type Refresh eff = Aff eff Unit

type Fd = FileDescriptor

getFdNothing :: forall eff. FilePath -> Aff eff (Tuple (Maybe Fd) (Refresh eff))
getFdNothing _ = pure (Tuple Nothing (pure unit))

withFdCache
  :: forall eff a
   . Number
  -> ((FilePath -> Aff (WaiEffects eff) (Tuple (Maybe Fd) (Refresh (WaiEffects eff))))
       -> Aff (WaiEffects eff) a)
  -> Aff (WaiEffects eff) a
withFdCache 0.00 action = action getFdNothing
withFdCache dur action = withResource (initialize dur) terminate (action <<< getFd)

data Status = Active | Inactive

newtype MutableStatus = MutableStatus (Ref Status)

status :: forall eff. MutableStatus -> Aff (ref :: REF | eff) Status
status (MutableStatus ref) = liftEff $ readRef ref

newActiveStatus :: forall eff. Aff (ref :: REF | eff) MutableStatus
newActiveStatus = MutableStatus <$> (liftEff $ newRef Active)

refresh :: forall eff. MutableStatus -> Refresh (ref :: REF | eff)
refresh (MutableStatus ref) = liftEff $ writeRef ref Active

inactive :: forall eff. MutableStatus -> Aff (ref :: REF | eff) Unit
inactive (MutableStatus ref) = liftEff $ writeRef ref Inactive

--------------------------------------------------------------------------------

data FdEntry = FdEntry FilePath Fd MutableStatus

openFile :: forall eff. FilePath -> Aff (fs :: FS | eff) Fd
openFile fp = F.fdOpen fp R Nothing

closeFile :: forall eff. Fd -> Aff (fs :: FS | eff) Unit
closeFile = F.fdClose

newFdEntry :: forall eff. FilePath -> Aff (fs :: FS, ref :: REF | eff) FdEntry
newFdEntry path = FdEntry path <$> openFile path <*> newActiveStatus

--------------------------------------------------------------------------------

type FdCache = M.Map FilePath FdEntry

newtype MutableFdCache eff = MutableFdCache (Reaper eff FdCache (Tuple FilePath FdEntry))

fdCache :: forall eff. MutableFdCache (WaiEffects eff) -> Aff (WaiEffects eff) FdCache
fdCache (MutableFdCache reaper) = reaperRead reaper

look :: forall eff. MutableFdCache (WaiEffects eff) -> FilePath -> Aff (WaiEffects eff) (Maybe FdEntry)
look mfc path = do
  v <- M.lookup path <$> fdCache mfc
  case v of
    Nothing -> pure Nothing
    Just (FdEntry path' _ _) -> pure $ if path /= path' then Nothing else v

--------------------------------------------------------------------------------

initialize :: forall eff. Number -> Aff (WaiEffects eff) (MutableFdCache (WaiEffects eff))
initialize dur = MutableFdCache <$> mkReaper settings
  where
  settings = ReaperSetting
    { action: clean
    , delay: wrap dur
    , cons: uncurry M.insert
    , isNull: M.isEmpty
    , empty: M.empty
    }

clean :: forall eff. FdCache -> Aff (WaiEffects eff) (FdCache -> FdCache)
clean old = do
  new <- map (hfilterMap id) $ traverse prune old
  pure $ M.union new
  where
  prune st@(FdEntry _ fd mst) = status mst >>= act
    where
    act Active   = inactive mst *> pure (Just st)
    act Inactive = F.fdClose fd *> pure Nothing

terminate :: forall eff. MutableFdCache (WaiEffects eff) -> Aff (WaiEffects eff) Unit
terminate (MutableFdCache reaper) = do
    t <- reaperStop reaper
    traverse_ closeIt $ (M.toAscUnfoldable t :: List (Tuple FilePath FdEntry))
  where
    closeIt (Tuple _ (FdEntry _ fd _)) = F.fdClose fd

hfilterMap :: forall k v v'. Ord k => (v -> Maybe v') -> M.Map k v -> M.Map k v'
hfilterMap p xs = foldr select M.empty (M.toUnfoldable xs :: List (Tuple k v))
  where
  select (Tuple k x) m = M.alter (const (p x)) k m

getFd
  :: forall eff
   . MutableFdCache (WaiEffects eff)
  -> FilePath
  -> Aff (WaiEffects eff) (Tuple (Maybe Fd) (Refresh (WaiEffects eff)))
getFd mfc@(MutableFdCache reaper) path = look mfc path >>= get
  where
  get Nothing = do
    ent@(FdEntry _ fd mst) <- newFdEntry path
    _ <- reaperAdd reaper (Tuple path ent)
    pure (Tuple (Just fd) (refresh mst))
  get (Just (FdEntry _ fd mst)) = do
    refresh mst
    pure (Tuple (Just fd) (refresh mst))
