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
  , Connection(..)
  , connSendMany
  , connSendAll
  , connSendFile
  , connWriteHead
  , connClose
  , connWriteBuffer
  , connBufferSize
  , InternalInfo0(..)
  , timeoutManager0
  , InternalInfo(..)
  , toInternalInfo
  , getFileInfo
  , getFd
  , timeoutHandle
  , SocketOption(..)
  ) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (Ref)
import Control.Monad.Eff.Exception (Error)

import Data.ArrayBuffer.TypedArray (Ptr, Uint8)
import Data.ByteString (ByteString)
import Data.List (List)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)

import Network.Wai (Request)
import Network.Wai.Handler.Node.FdCache as F
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
  , connWriteHead   :: H.Status -> List H.Header -> Aff eff Unit
  , connWriteBuffer :: Buffer
  , connBufferSize  :: BufSize
  }

connSendMany :: forall eff. Connection eff -> List ByteString -> Aff eff Unit
connSendMany (Connection r) = r.connSendMany

connSendAll :: forall eff. Connection eff -> ByteString -> Aff eff Unit
connSendAll (Connection r) = r.connSendAll

connSendFile :: forall eff. Connection eff -> SendFile eff
connSendFile (Connection r) = r.connSendFile

connWriteHead :: forall eff. Connection eff -> H.Status -> List H.Header -> Aff eff Unit
connWriteHead (Connection r) = r.connWriteHead

connClose :: forall eff. Connection eff -> Aff eff Unit
connClose (Connection r) = r.connClose

connWriteBuffer :: forall eff. Connection eff -> Buffer
connWriteBuffer (Connection r) = r.connWriteBuffer

connBufferSize :: forall eff. Connection eff -> BufSize
connBufferSize (Connection r) = r.connBufferSize

data InternalInfo0 eff =
  InternalInfo0
    (T.Manager eff)
    (FilePath -> Aff eff (Tuple (Maybe F.Fd) (F.Refresh eff)))
    (FilePath -> Aff eff I.FileInfo)

timeoutManager0 :: forall eff. InternalInfo0 eff -> T.Manager eff
timeoutManager0 (InternalInfo0 m _ _) = m

data InternalInfo eff =
  InternalInfo
    (T.Handle eff)
    (T.Manager eff)
    (FilePath -> Aff eff (Tuple (Maybe F.Fd) (F.Refresh eff)))
    (FilePath -> Aff eff I.FileInfo)

toInternalInfo :: forall eff. T.Handle eff -> InternalInfo0 eff -> InternalInfo eff
toInternalInfo h (InternalInfo0 m f g) = InternalInfo h m f g

timeoutHandle :: forall eff. InternalInfo eff -> T.Handle eff
timeoutHandle (InternalInfo h _ _ _) = h

getFileInfo :: forall eff. InternalInfo eff -> FilePath -> Aff eff I.FileInfo
getFileInfo (InternalInfo _ _ _ gt) = gt

getFd :: forall eff. InternalInfo eff -> FilePath -> Aff eff (Tuple (Maybe F.Fd) (F.Refresh eff))
getFd (InternalInfo _ _ ft _) = ft

data SocketOption = SockTCP String Int (Maybe Int) | SockUnix String

newtype Settings eff = Settings
  { socketOption          :: SocketOption
  , timeout               :: Number
  , manager               :: Maybe (T.Manager eff)
  , fdCacheDuration       :: Number
  , fileInfoCacheDuration :: Number
  , logger                :: Request eff -> H.Status -> Maybe Int -> Aff eff Unit
  , onException           :: Maybe (Request eff) -> Error -> Eff eff Unit
  , slowlorisSize         :: Int
  }

settingsLogger :: forall eff. Settings eff -> Request eff -> H.Status -> Maybe Int -> Aff eff Unit
settingsLogger (Settings { logger }) = logger
