module Network.Wai.Handler.Node.Recv
  ( receive
  , receiveBuf
  , makeReceiveN
  , spell
  ) where

import Prelude

import Control.Monad.Aff (Aff, effCanceler, makeAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION, Error)
import Control.Monad.Eff.Ref (REF, Ref, newRef, writeRef, readRef)

import Data.ByteString (ByteString(..), length, splitAt, empty)
import Data.ByteString.Node.Buffer (fromBuffer)
import Data.ByteString.Internal (concat)
import Data.Either (Either(..))
import Data.Function.Uncurried as Fn
import Data.List (List(Nil), (:), reverse)
import Data.Tuple (Tuple(..))

import Node.Buffer as NB
import Node.Stream (Readable)

import Network.Wai.Handler.Node.Buffer (mallocBS, copy, withBufferPool)
import Network.Wai.Handler.Node.Types (BufferPool, Recv, RecvBuf, BufSize)


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
  _ <- liftEff $ writeRef ref leftover
  pure bs

spell
  :: forall eff
   . ByteString
  -> BufSize
  -> Aff eff ByteString
  -> RecvBuf eff
  -> Aff eff { bs :: ByteString, leftover :: ByteString }
spell init0 siz0 recv recvBuf =
  if siz0 <= len0
    then
      let { before, after } = splitAt siz0 init0
      in pure $ { bs: before, leftover: after }
    else if siz0 <= 4096
      then loop (init0 : Nil) (siz0 - len0)
      else do
        bs@(ByteString ptr _ _) <- liftEff $ mallocBS siz0
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
      if len == 0
        then pure $ { bs: empty, leftover: empty }
        else if len >= siz
          then do
            let { before, after } = splitAt siz bs
                ret = concat $ reverse (before : bss)
            pure $ { bs: ret, leftover: after }
          else do
            let bss' = bs : bss
                siz' = siz - len
            loop bss' siz'

receive
  :: forall w eff
   . Readable w (exception :: EXCEPTION, ref :: REF | eff)
  -> BufferPool
  -> Recv (exception :: EXCEPTION, ref :: REF | eff)
receive stream pool = withBufferPool pool \(Tuple buf size) -> do
  nbuf <- receiveStream stream size
  let bs  = fromBuffer nbuf
      len = length bs
  if len == 0
    then pure len
    else do
      _ <- liftEff $ copy buf bs
      pure len

receiveBuf
  :: forall w eff
   . Readable w (exception :: EXCEPTION, ref :: REF | eff)
  -> RecvBuf (exception :: EXCEPTION, ref :: REF | eff)
receiveBuf stream buf0 siz0 = go buf0 siz0
  where
  go _ 0 = pure true
  go buf size = do
    nbuf <- receiveStream stream size
    let bs  = fromBuffer nbuf
        len = length bs
    if len == 0
      then pure false
      else do
        buf' <- liftEff $ copy buf bs
        go buf' (size - len)

receiveStream
  :: forall w eff
   . Readable w (exception :: EXCEPTION | eff)
  -> Int
  -> Aff (exception :: EXCEPTION | eff) NB.Buffer
receiveStream stream size = makeAff \k -> do
  c <- Fn.runFn5 _receiveStream Left Right stream size k
  pure (effCanceler c)

foreign import _receiveStream
  :: forall w eff
   . Fn.Fn5
      (forall x y. x -> Either x y)
      (forall x y. y -> Either x y)
      (Readable w (exception :: EXCEPTION | eff))
      Int
      (Either Error NB.Buffer -> Eff (exception :: EXCEPTION | eff) Unit)
      (Eff (exception :: EXCEPTION | eff) (Eff (exception :: EXCEPTION | eff) Unit))
