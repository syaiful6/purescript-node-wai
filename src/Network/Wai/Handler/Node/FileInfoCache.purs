module Network.Wai.Handler.Node.FileInfoCache
  ( FileInfo(..)
  , withFileInfoCache
  , getInfo
  ) where

import Prelude

import Control.Monad.Aff (Aff, bracket)
import Control.Monad.Aff.Schedule.Reaper (Reaper, mkReaper, ReaperSetting(..)
                                         ,reaperRead, reaperAdd, reaperStop)
import Control.Monad.Error.Class (throwError, catchError)
import Control.Monad.Eff.Exception (error)

import Data.Map as M
import Data.Int (fromNumber)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (wrap)
import Data.Tuple (Tuple(..), uncurry)

import Network.Wai.Types.Date (HTTPDate, fromDateTime, formatHTTPDate)
import Node.FS.Aff as F
import Node.FS.Stats (Stats(..), isFile, modifiedTime)
import Node.Path (FilePath)

import Network.Wai.Handler.Node.Effects (WaiEffects)

import Partial.Unsafe (unsafePartial)


newtype FileInfo = FileInfo
  { name :: FilePath
  , size :: Int
  , time :: HTTPDate -- ^ Modification time
  , date :: String -- ^ Modification time in the GMT format
  }

instance eqFileInfo :: Eq FileInfo where
  eq (FileInfo s1) (FileInfo s2) = s1.name == s2.name

instance ordFileInfo :: Ord FileInfo where
  compare (FileInfo s1) (FileInfo s2) = compare s1.name s2.name

data Entry = Negative | Positive FileInfo

type Cache = M.Map FilePath Entry

type FileInfoCache eff = Reaper eff Cache (Tuple FilePath Entry)

getInfo :: forall eff. FilePath -> Aff (fs :: F.FS | eff) FileInfo
getInfo fp = do
  fs <- F.stat fp
  if isFile fs
    then
      let time = fromDateTime $ modifiedTime fs
          date = formatHTTPDate time
      in pure (FileInfo { name: fp, size: fileSizeInfo fs, time: time, date: date })
    else throwError (error ("getInfo: " <> fp <> " isn't a file"))

fileSizeInfo :: Stats -> Int
fileSizeInfo (Stats s) = unsafePartial $ fromJust (fromNumber s.size)

getAndRegisterInfo
  :: forall eff
   . FileInfoCache (fs :: F.FS | eff)
  -> FilePath
  -> Aff (fs :: F.FS | eff) FileInfo
getAndRegisterInfo fic path = do
  cache <- reaperRead fic
  case M.lookup path cache of
    Just Negative      -> throwError (error ("FileInfoCache:getAndRegisterInfo"))
    Just (Positive fi) -> pure fi
    Nothing            -> positive fic path `catchError` \_ -> negative fic path

positive
  :: forall eff
   . FileInfoCache (fs :: F.FS | eff)
  -> FilePath
  -> Aff (fs :: F.FS | eff) FileInfo
positive fic path = do
  info <- getInfo path
  _ <- reaperAdd fic (Tuple path (Positive info))
  pure info

negative :: forall eff. FileInfoCache eff -> FilePath -> Aff eff FileInfo
negative fic path = do
  _ <- reaperAdd fic (Tuple path Negative)
  throwError (error "FileInfoCache:negative")

withFileInfoCache
  :: forall eff a
   . Number
  -> ((FilePath -> Aff (WaiEffects eff) FileInfo) -> Aff (WaiEffects eff) a)
  -> Aff (WaiEffects eff) a
withFileInfoCache 0.00 action = action getInfo
withFileInfoCache dur action  = bracket (initialize dur) terminate (action <<< getAndRegisterInfo)

initialize :: forall eff. Number -> Aff (WaiEffects eff) (FileInfoCache (WaiEffects eff))
initialize dur = mkReaper $ ReaperSetting
  { action: override
  , delay: wrap dur
  , cons: uncurry M.insert
  , isNull: M.isEmpty
  , empty: M.empty
  }

override :: forall eff. Cache -> Aff eff (Cache -> Cache)
override _ = pure $ const M.empty

terminate :: forall eff. FileInfoCache eff -> Aff eff Unit
terminate x = void $ reaperStop x
