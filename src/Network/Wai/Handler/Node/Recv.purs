module Network.Wai.Handler.Node.Recv where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (REF, Ref, newRef, writeRef, readRef)

import Data.ByteString (ByteString(..), length, splitAt, empty)
import Data.ByteString.Internal (concat)
import Data.List (List(Nil), (:), reverse)

import Node.Stream

import Network.Wai.Handler.Node.BufferPool (mallocBS, copy)
import Network.Wai.Handler.Node.Types (Buffer, Recv, RecvBuf, BufSize)


makeReceiveN
  :: forall eff
   . ByteString
  -> Recv (ref :: REF | eff)
  -> RecvBuf (ref :: REF | eff)
  -> Aff (ref :: REF | eff) (BufSize -> Aff (ref :: REF | eff) ByteString)
makeReceiveN bs recv recvBuf = do
  ref <- liftEff $ newRef bs
  pure $ receiveN ref recv recvBuf

receiveN
  :: forall eff
   . Ref ByteString
  -> Recv (ref :: REF | eff)
  -> RecvBuf (ref :: REF | eff)
  -> BufSize
  -> Aff (ref :: REF | eff) ByteString
receiveN ref recv recvBuf size = do
  cached <- liftEff $ readRef ref
  { bs, leftover } <- spell cached size recv recvBuf
  _ <- liftEff $ writeRef leftover
  pure bs

spell
  :: ByteString
  -> BufSize eff
  -> Aff eff ByteString
  -> RecvBuf eff
  -> Aff eff { bs :: ByteString, leftover :: ByteString }
spell init0 siz0 recv recvBuf =
  if siz0 <= len0 then
    let { before, after } = splitAt siz0 init0
    in pure $ { bs: before, leftover :: after }
  else if siz0 <= 4096 then
    loop (init0 : Nil) (siz0 - len0)
  else do
    bs@(PS ptr _ _) <- liftEff $ mallocBS siz0
    ptr' <- liftEff $ copy ptr init0
    full <- recvBuf ptr' (siz0 - len0)
    pure $
      if full
        then { bs: bs, leftover: empty }
        else { bs: empty, leftover: empty }
  where
    len0 = length init0
    loop bss siz = do
      bs <- recv
      let len = length bs
      if len == 0 then
        { bs: empty, leftover: empty }
      else if len >= siz then do
        let { before, after } = splitAt siz bs
            ret = concat $ reverse (before : bss)
        pure $ { bs: ret, leftover: after }
      else do
        let bss' = bs : bss
            siz' = siz - len
        loop bss' siz'
