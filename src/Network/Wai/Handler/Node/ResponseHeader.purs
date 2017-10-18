module Network.Wai.Handler.Node.ResponseHeader (composeHeader) where

import Prelude

import Control.Monad.Eff (Eff, forE)
import Control.Monad.Rec.Class (Step(..), tailRecM2)

import Data.ArrayBuffer.Types (Uint8)
import Data.Foldable (foldl)
import Data.List (List(Nil), (:))
import Data.Tuple (Tuple(..))
import Data.ByteString as B
import Data.ByteString.Internal (Ptr, create, memcpyArr, plusPtr, pokeByteOff)
import Data.Newtype (unwrap)
import Data.Int.Bits ((.&.))

import Data.String (length) as S
import Data.String.Unsafe (charCodeAt) as S

import Network.Wai.Types as H
import Network.Wai.Handler.Node.Buffer (copy)


composeHeader :: forall eff. H.HttpVersion -> H.Status -> H.ResponseHeaders -> Eff eff B.ByteString
composeHeader ver status hdrs = create len \ptr -> do
    ptr1 <- copyStatus ptr ver status
    ptr2 <- copyHeaders ptr1 hdrs
    void $ copyCRLF ptr2
  where
  len  = 17 + slen + foldl fieldLength 0 hdrs
  fieldLength l (Tuple k v) = l + S.length (unwrap k) + S.length v + 4
  slen = S.length (H.statusMessage status)

httpVer11 :: B.ByteString
httpVer11 = B.pack [72, 84, 84, 80, 47, 49, 46, 49, 32]

httpVer10 :: B.ByteString
httpVer10 = B.pack [72, 84, 84, 80, 47, 49, 46, 48, 32]

copyStatus :: forall eff. Ptr Uint8 -> H.HttpVersion -> H.Status -> Eff eff (Ptr Uint8)
copyStatus ptr ver status = do
  ptr1 <- copy ptr httpVer
  _ <- memcpyArr ptr1 [48 + r2, 48 + r1, 48 + r0, 32]
  ptr2 <- copyStrChar8 (ptr1 `plusPtr` 4) (H.statusMessage status)
  copyCRLF ptr2
  where
  httpVer
      | ver == H.HttpVersion 1 1 = httpVer11
      | otherwise                = httpVer10
  sCode = H.statusNumber status
  q0 = sCode `div` 10
  r0 = sCode `mod` 10
  q1 = q0 `div` 10
  r1 = q0 `mod` 10
  r2 = q1 `mod` 10

copyHeaders :: forall eff. Ptr Uint8 -> List H.Header -> Eff eff (Ptr Uint8)
copyHeaders ptr0 xs0 = tailRecM2 go ptr0 xs0
  where
  go ptr Nil    = pure (Done ptr)
  go ptr (h:hs) = do
    ptr1 <- copyHeader ptr h
    pure $ Loop { a: ptr1, b: hs }

copyHeader :: forall eff. Ptr Uint8 -> H.Header -> Eff eff (Ptr Uint8)
copyHeader ptr (Tuple k v) = do
  ptr1 <- copyStrChar8 ptr (unwrap k)
  _ <- memcpyArr ptr1 [58, 32]
  ptr2 <- copyStrChar8 (ptr1 `plusPtr` 2) v
  copyCRLF ptr2

copyCRLF :: forall eff. Ptr Uint8 -> Eff eff (Ptr Uint8)
copyCRLF ptr = do
  _ <- memcpyArr ptr [13, 10]
  pure (ptr `plusPtr` 2)

copyStrChar8 :: forall eff. Ptr Uint8 -> String -> Eff eff (Ptr Uint8)
copyStrChar8 ptr msg = do
  _ <- forE 0 lenxs \i -> pokeByteOff ptr i (S.charCodeAt i msg .&. 0xFF)
  pure $ ptr `plusPtr` lenxs
  where
    lenxs = S.length msg
