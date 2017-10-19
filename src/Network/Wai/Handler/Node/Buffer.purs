module Network.Wai.Handler.Node.Buffer
  ( bufferSize
  , allocateBuffer
  , mallocBS
  , newBufferPool
  , withBufferPool
  , copy
  , bufferAff
  , toBuilderBuffer
  , chunkedTransferEncoding
  , chunkedTransferTerminator
  , toBufAffWith
  , word32HexLength
  , loopUntilZero
  ) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (REF, newRef, writeRef, readRef)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Eff.Exception (error)

import Data.ByteString as B
import Data.ByteString.Internal (ByteString(..), mallocByteString, plusPtr, minusPtr, memcpy, memcpyArr, poke)
import Data.ByteString.Builder.Extra (runBuilder, refinedEff, Next(..))
import Data.ByteString.Builder (string7)
import Data.ByteString.Builder.Internal as BI
import Data.ByteString.Unsafe (unsafeDrop, unsafeTake)
import Data.Int.Bits ((.&.), shr)
import Data.Tuple (Tuple(..))

import Network.Wai.Handler.Node.Types (Buffer, BufferPool, BufSize)
import Network.Wai.Handler.Node.Utils (nextTick)

bufferSize :: BufSize
bufferSize = 16384

allocateBuffer :: forall eff. Int -> Eff eff Buffer
allocateBuffer = mallocByteString

largeBufferSize :: Int
largeBufferSize = 16384

minBufferSize :: Int
minBufferSize = 2048

newBufferPool :: forall eff. Eff (ref :: REF | eff) BufferPool
newBufferPool = newRef B.empty

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
copy ptr (ByteString p o l) = do
  _ <- memcpy ptr (p `plusPtr` o) l
  pure $ ptr `plusPtr` l

bufferAff :: forall eff. Buffer -> Int -> (ByteString -> Aff eff Unit) -> Aff eff Unit
bufferAff ptr siz io = io $ ByteString ptr 0 siz

toBuilderBuffer :: Buffer -> BufSize -> BI.Buffer
toBuilderBuffer ptr size = BI.Buffer ptr (BI.BufferRange ptr (ptr `plusPtr` size))

chunkedTransferEncoding :: BI.Builder -> BI.Builder
chunkedTransferEncoding innerBuilder = BI.builder transferEncodingStep
  where
  transferEncodingStep :: forall r. BI.BuildStep r -> BI.BuildStep r
  transferEncodingStep k = go (BI.runBuilder innerBuilder)
    where
    go innerStep (BI.BufferRange op ope) =
      if outRemaining < minimalBufferSize
        then pure $ BI.bufferFull minimalBufferSize op (go innerStep)
        else
          let brInner@(BI.BufferRange opInner _) = BI.BufferRange
                    (op  `plusPtr` (chunkSizeLength + 2))     -- leave space for chunk header
                    (ope `plusPtr` (-maxAfterBufferOverhead))

              wrapChunk opInner' mkSignal
                | opInner' == opInner = mkSignal op
                | otherwise           = do
                    _ <- liftEff do
                      _ <- pokeWord32HexN chunkSizeLength (opInner' `minusPtr` opInner) op
                      _ <- copyCRLF (opInner `plusPtr` (-2))
                      copyCRLF opInner'
                    mkSignal (opInner' `plusPtr` 2)

              doneH opInner' _ = wrapChunk opInner' $ \op' ->
                let br' = BI.BufferRange op' ope
                in k br'

              fullH opInner' minRequiredSize nextInnerStep =
                wrapChunk opInner' \op' ->
                  pure $ BI.bufferFull
                    (minRequiredSize + maxEncodingOverhead)
                    op'
                    (go nextInnerStep)

              insertChunkH opInner' bs nextInnerStep
                | B.null bs =
                    wrapChunk opInner' \op' ->
                      pure $ BI.insertChunk op' B.empty (go nextInnerStep)
                | otherwise =
                    wrapChunk opInner' \op' -> do
                      let
                        w  = B.length bs
                        len' = word32HexLength w
                      _ <- liftEff $ pokeWord32HexN len' w op'
                      ptr' <- liftEff $ copyCRLF (op' `plusPtr` len')
                      pure $ BI.insertChunk ptr' bs (BI.runBuilderWith (string7 "\r\n") $ go nextInnerStep)

          in BI.fillWithBuildStep innerStep doneH fullH insertChunkH brInner
      where
      maxBeforeBufferOverhead = 10
      maxAfterBufferOverhead  = 12
      minimalChunkSize        = 1
      maxEncodingOverhead     = maxBeforeBufferOverhead + maxAfterBufferOverhead
      minimalBufferSize       = minimalChunkSize + maxEncodingOverhead
      outRemaining            = ope `minusPtr` op
      chunkSizeLength         = word32HexLength outRemaining

chunkedTransferTerminator :: BI.Builder
chunkedTransferTerminator = string7 "0\r\n\r\n"

loopUntilZero :: (Int -> Int) -> Int -> Int
loopUntilZero f = go 0
  where
  go a 0 = a
  go a x = go (a + 1) (f x)

word32HexLength :: Int -> Int
word32HexLength = max 1 <<< loopUntilZero (_ `shr` 4)

pokeWord32HexN :: forall eff. Int -> Int -> Buffer -> Eff eff Unit
pokeWord32HexN n0 w0 op0 =
  go w0 (op0 `plusPtr` (n0 - 1))
  where
  go w op
    | op < op0  = pure unit
    | otherwise = do
        let nibble = w .&. 0xF
            hex | nibble < 10 = 48 + nibble
                | otherwise   = 55 + nibble
        poke op hex
        go (w `shr` 4) (op `plusPtr` (-1))

copyCRLF :: forall eff. Buffer -> Eff eff Buffer
copyCRLF ptr = do
  _ <- memcpyArr ptr [13, 10]
  pure (ptr `plusPtr` 2)

toBufAffWith
  :: forall eff
   . Buffer
  -> BufSize
  -> (ByteString -> Aff eff Unit)
  -> BI.Builder
  -> Aff eff Unit
toBufAffWith buf size io builder = go firstWriter
  where
  firstWriter = runBuilder builder
  runIO len   = bufferAff buf len io
  go writer = do
    Tuple len signal <- liftEff $ refinedEff $ writer buf size
    case signal of
      Done -> runIO len
      More minSize next
        | size < minSize -> throwError (error "toBufIOWith: BufferFull: minSize")
        | otherwise      -> do
            _ <- runIO len
            _ <- nextTick
            go next
      Chunk bs next -> do
        _ <- runIO len
        _ <- io bs
        _ <- nextTick
        go next
