module Network.Wai.Handler.Node.Types
  ( BufferPool
  , Buffer
  , BufSize
  , Recv
  , RecvBuf
  , FileId(..)
  , SendFile
  , Settings(..)
  , settingsLogger
  , defaultSettings
  , Connection(..)
  , InternalInfo0(..)
  , timeoutManager0
  , InternalInfo(..)
  , toInternalInfo
  , getFileInfo
  , getFd
  , timeoutHandle
  , getDate
  , Source(..)
  , mkSource
  , readSource
  , readSource'
  , leftoverSource
  , readLeftoverSource
  , SocketOption(..)
  ) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.Monad.Eff.Ref (REF,Ref, newRef, readRef, writeRef)
import Control.Monad.Eff.Exception (Error)

import Data.ArrayBuffer.Types (Uint8)
import Data.ByteString (ByteString)
import Data.ByteString as B
import Data.ByteString.Builder (string7)
import Data.ByteString.Internal (Ptr)
import Data.List (List(Nil), (:))
import Data.Maybe (Maybe(Nothing))
import Data.Tuple (Tuple(..))

import Network.Wai (responseBuilder, Request, Response)
import Network.Wai.Handler.Node.FdCache as F
import Network.Wai.Handler.Node.Date as D
import Network.Wai.Handler.Node.FileInfoCache as I
import Network.Wai.Handler.Node.Timeout as T
import Network.Wai.Types as H

import Node.Path (FilePath)
import Node.FS (FileDescriptor)


type BufferPool = Ref ByteString

type Buffer = Ptr Uint8

type BufSize = Int

type Recv eff = Aff eff ByteString

type RecvBuf eff = Buffer -> BufSize -> Aff eff Boolean

newtype FileId = FileId
  { fileIdPath :: FilePath
  , fileIdFd   :: Maybe FileDescriptor
  }

type SendFile eff = FileId -> Int -> Int -> Aff eff Unit -> Aff eff Unit

newtype Connection eff = Connection
  { connSendMany    :: List ByteString -> Aff eff Unit
  , connSendAll     :: ByteString -> Aff eff Unit
  , connSendFile    :: SendFile eff
  , connClose       :: Aff eff Unit
  , connRecv        :: Recv eff
  , connRecvBuf     :: RecvBuf eff
  , connWriteBuffer :: Buffer
  , connBufferSize  :: BufSize
  }

data InternalInfo0 eff =
  InternalInfo0
    (T.Manager eff)
    (Aff eff D.GMTDate)
    (FilePath -> Aff eff (Tuple (Maybe F.Fd) (F.Refresh eff)))
    (FilePath -> Aff eff I.FileInfo)

timeoutManager0 :: forall eff. InternalInfo0 eff -> T.Manager eff
timeoutManager0 (InternalInfo0 m _ _) = m

data InternalInfo eff =
  InternalInfo
    (T.Handle eff)
    (T.Manager eff)
    (Aff eff D.GMTDate)
    (FilePath -> Aff eff (Tuple (Maybe F.Fd) (F.Refresh eff)))
    (FilePath -> Aff eff I.FileInfo)

toInternalInfo :: forall eff. T.Handle eff -> InternalInfo0 eff -> InternalInfo eff
toInternalInfo h (InternalInfo0 m d f g) = InternalInfo h m d f g

timeoutHandle :: forall eff. InternalInfo eff -> T.Handle eff
timeoutHandle (InternalInfo h _ _ _ _) = h

getFileInfo :: forall eff. InternalInfo eff -> FilePath -> Aff eff I.FileInfo
getFileInfo (InternalInfo _ _ _ _ gt) = gt

getFd :: forall eff. InternalInfo eff -> FilePath -> Aff eff (Tuple (Maybe F.Fd) (F.Refresh eff))
getFd (InternalInfo _ _ _ ft _) = ft

getDate :: forall eff. InternalInfo eff -> Aff eff D.GMTDate
getDate (InternalInfo _ _ d _ _) = d

data Source eff = Source (Ref ByteString) (Aff eff ByteString)

mkSource :: forall eff. Aff (ref :: REF | eff) ByteString -> Aff (ref :: REF | eff) (Source (ref :: REF | eff))
mkSource aff = do
  ref <- liftEff $ newRef B.empty
  pure $ Source ref aff

readSource :: forall eff. Source (ref :: REF | eff) -> Aff (ref :: REF | eff) ByteString
readSource (Source ref aff) = do
  bs <- liftEff $ readRef ref
  if B.null bs
    then aff
    else do
      _ <- liftEff $ writeRef ref B.empty
      pure bs

-- | Read from a Source, ignoring any leftovers.
readSource' :: forall eff. Source (ref :: REF | eff) -> Aff (ref :: REF | eff) ByteString
readSource' (Source _ aff) = aff

leftoverSource :: forall eff. Source (ref :: REF | eff) -> ByteString -> Aff (ref :: REF | eff) Unit
leftoverSource (Source ref _) bs = liftEff $ writeRef ref bs

readLeftoverSource :: forall eff. Source (ref :: REF | eff) -> Aff (ref :: REF | eff) ByteString
readLeftoverSource (Source ref _) = liftEff $ readRef ref

data SocketOption = SockTCP String Int (Maybe Int) | SockUnix String

newtype Settings eff = Settings
  { socketOption          :: SocketOption
  , timeout               :: Number
  , manager               :: Maybe (T.Manager eff)
  , fdCacheDuration       :: Number
  , fileInfoCacheDuration :: Number
  , logger                :: Request eff -> H.Status -> Maybe Int -> Aff eff Unit
  , onException           :: Maybe (Request eff) -> Error -> Eff eff Unit
  , onExceptionResponse   :: Error -> Response eff
  , slowlorisSize         :: Int
  }

defaultSettings :: forall eff. Settings (console :: CONSOLE | eff)
defaultSettings = Settings
  { socketOption: SockTCP "127.0.0.1" 3000 Nothing
  , timeout: 30.00
  , manager: Nothing
  , fdCacheDuration: 0.00
  , fileInfoCacheDuration: 0.00
  , logger: \_ _ _ -> pure unit
  , onException: defaultOnException
  , onExceptionResponse: defaultOnExceptionResponse
  , slowlorisSize: 2048
  }

settingsLogger :: forall eff. Settings eff -> Request eff -> H.Status -> Maybe Int -> Aff eff Unit
settingsLogger (Settings { logger }) = logger

defaultOnExceptionResponse :: forall eff. Error -> Response eff
defaultOnExceptionResponse _ = responseBuilder
  H.internalServerError500
  ((Tuple H.hContentType "text/plain; charset=utf-8") : Nil)
  (string7 "Something went wrong")

defaultOnException
  :: forall eff
   . Maybe (Request (console :: CONSOLE | eff))
  -> Error
  -> Eff (console :: CONSOLE | eff) Unit
defaultOnException _ e = logShow e
