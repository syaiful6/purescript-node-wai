module Network.Wai.Handler.Node.Types
  ( BufferPool
  , Buffer
  , BufSize
  , FileId(..)
  , SendFile
  , Connection(..)
  , InternalInfo(..)
  ) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Ref (Ref)

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

newtype FileId = FileId
  { fileIdPath :: FilePath
  , fileIdFd   :: Maybe FileDescriptor
  }

type SendFile eff = FileId -> Int -> Int -> Aff eff Unit -> H.ResponseHeaders -> Aff eff Unit

newtype Connection eff = Connection
  { connSendMany    :: List ByteString -> Aff eff Unit
  , connSendAll     :: ByteString -> Aff eff Unit
  , connSendFile    :: SendFile eff
  , connWriteHead   :: List H.Header -> Aff eff Unit
  , connWriteBuffer :: Buffer
  , connBufferSize  :: BufSize
  }

connSendMany :: forall eff. Connection eff -> List ByteString -> Aff eff Unit
connSendMany (Connection r) = r.connSendMany

connSendAll :: forall eff. Connection eff -> ByteString -> Aff eff Unit
connSendAll (Connection r) = r.connSendAll

connSendFile :: forall eff. Connection eff -> SendFile eff
connSendFile (Connection r) = r.connSendFile

connWriteHead :: forall eff. Connection eff -> List H.Header -> Aff eff Unit
connWriteHead (Connection r) = r.connWriteHead

newtype InternalInfo eff = InternalInfo
  { threadHandle   :: T.Handle eff
  , timeoutManager :: T.Manager eff
  , getFd          :: FilePath -> Aff eff (Tuple (Maybe F.Fd) (F.Refresh eff))
  , getFileInfo    :: FilePath -> Aff eff I.FileInfo
  }

newtype Settings eff = Settings
  { timeout               :: Number
  , manager               :: Maybe (T.Manager eff)
  , fdCacheDuration       :: Number
  , fileInfoCacheDuration :: Number
  , logger                :: Request eff -> H.Status -> Maybe Int -> Aff eff Unit
  }
