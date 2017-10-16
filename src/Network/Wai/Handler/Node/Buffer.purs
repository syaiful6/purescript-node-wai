module Network.Wai.Handler.Node.BufferPool where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (REF, newRef, writeRef, readRef)

import Data.ByteString as B
import Data.ByteString.Internal (ByteString(..), mallocByteString, newPtr, plusPtr, memcpy)
import Data.ByteString.Unsafe (unsafeDrop, unsafeTake)
import Data.Tuple (Tuple(..))

import Network.Wai.Handler.Node.Types (Buffer, BufferPool, BufSize)


bufferSize :: BufSize
bufferSize = 16384

allocateBuffer :: forall eff. Int -> Eff eff Buffer
allocateBuffer = mallocByteString

largeBufferSize :: Int
largeBufferSize = 16384

minBufferSize :: Int
minBufferSize = 2048

newBufferPool :: forall eff. Eff (ref :: REF | eff) BufferPool
newBufferPool = newRef BS.empty

mallocBS :: forall eff. Int -> Eff eff ByteString
mallocBS size = do
  ptr <- allocateBuffer size
  pure $ B.ByteString ptr 0 size

usefulBuffer :: ByteString -> Boolean
usefulBuffer bs = B.length bs >= minBufferSize

getBuffer :: forall eff. BufferPool -> Eff (ref :: REF | eff) ByteString
getBuffer pool = do
  buffer <- readRef pool
  if usefulBuffer buffer then pure buffer else mallocBS largeBufferSize

putBuffer :: forall eff. BufferPool -> ByteString -> Eff (ref :: REF | eff) Unit
putBuffer pool bs = writeRef pool bs

withBufferPool
  :: forall eff
   . BufferPool
  -> (Tuple Buffer BufSize -> Aff (ref :: REF | eff) Int)
  -> Aff (ref :: REF | eff) ByteString
withBufferPool pool f = do
  buff@B.ByteString p s l <- liftEff $ getBuffer pool
  consumed <- f (Tuple (p `plusPtr` s) l)
  _ <- liftEff $ putBuffer pool (unsafeDrop consumed buff)
  pure $ unsafeTake consumed buff

copy :: forall eff. Buffer -> ByteString -> Eff eff Buffer
copy buf (ByteString ptr o l) = do
  _ <- memcpy buf (ptr `plusPtr` o) l
  pure $ ptr `plusPtr` l
