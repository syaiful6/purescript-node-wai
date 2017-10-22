module Network.Wai.Handler.Node.Source where

import Prelude

import Control.Monad.Aff (Aff, throwError, error)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (REF, Ref, newRef, readRef, writeRef)

import Data.ByteString as S
import Data.Maybe (Maybe(..))

import Network.Wai.Handler.Node.Types (Source, readSource, readSource', leftoverSource)
import Network.Wai.Handler.Node.Utils (nextTick)

data ISource eff = ISource (Source eff) (Ref Int)

mkISource
  :: forall eff
   . Source (ref :: REF | eff)
  -> Int
  -> Eff (ref :: REF | eff) (ISource (ref :: REF | eff))
mkISource src i = do
  ri <- newRef i
  pure (ISource src ri)

readISource :: forall eff. ISource (ref :: REF | eff) -> Aff (ref :: REF | eff) S.ByteString
readISource (ISource src ref) = do
  count <- liftEff $ readRef ref
  if count == 0
    then pure S.empty
    else do
      bs <- readSource src
      when (S.null bs) (throwError (error "ConnectionClosedByPeer"))
      let
        toSend = min count (S.length bs)
        count' = count - toSend
      if count' > 0
        then do
          liftEff $ writeRef ref count'
          pure bs
        else do
          let { before, after } = S.splitAt toSend bs
          leftoverSource src after
          unless (count' == 0) (throwError (error "assertion failed"))
          liftEff $ writeRef ref count'
          pure before

data CSource eff = CSource (Source eff) (Ref ChunkState)

data ChunkState
  = NeedLen
  | NeedLenNewline
  | HaveLen Int
  | DoneChunking

instance showChunkState :: Show ChunkState where
  show = case _ of
    NeedLen        -> "DoneChunking"
    NeedLenNewline -> "DoneChunking"
    HaveLen i      -> "(HaveLen " <> show i <> " )"
    DoneChunking   -> "DoneChunking"

mkCSource
  :: forall eff
   . Source (ref :: REF | eff)
  -> Eff (ref :: REF | eff) (CSource (ref :: REF | eff))
mkCSource src = do
  ref <- newRef NeedLen
  pure $ CSource src ref

readCSource :: forall eff. CSource (ref :: REF | eff) -> Aff (ref :: REF | eff) S.ByteString
readCSource (CSource src ref) = do
  mlen <- liftEff $ readRef ref
  go mlen
  where
    withLen 0 bs = do
      _ <- leftoverSource src bs
      _ <- dropCRLF
      yield' S.empty DoneChunking
    withLen len bs
      | S.null bs = do
          liftEff $ writeRef ref DoneChunking
          pure S.empty
      | otherwise = case S.length bs `compare` len of
          EQ -> yield' bs NeedLenNewline
          LT -> yield' bs $ HaveLen $ len - S.length bs
          GT -> do
            let { before, after } = S.splitAt len bs
            _ <- leftoverSource src after
            yield' before NeedLenNewline

    yield' bs mlen = liftEff do
      writeRef ref mlen
      pure bs

    dropCRLF = do
      bs <- readSource src
      case S.uncons bs of
        Nothing               -> pure unit
        Just { head, tail }
          | head == 13        -> dropLF tail
          | head == 10        -> leftoverSource src tail
          | otherwise         -> leftoverSource src bs

    dropLF bs = case S.uncons bs of
      Nothing -> do
        bs2 <- readSource' src
        unless (S.null bs2) do
          _ <- nextTick
          dropLF bs2
      Just { head, tail }
        | head == 10 -> leftoverSource src tail
        | otherwise  -> leftoverSource src bs

    go NeedLen = getLen
    go NeedLenNewline = dropCRLF *> getLen
    go (HaveLen 0) = do
      _ <- dropCRLF
      liftEff $ writeRef ref DoneChunking
      pure S.empty
    go (HaveLen len) = do
      bs <- readSource src
      withLen len bs
    go DoneChunking = pure S.empty

    mayBreak bs = case S.break ((==) 10) bs of
      { before, after }
        | S.null after -> do
            bs2 <- readSource' src
            pure $ if S.null bs2
              then { before, after }
              else S.break ((==) 10) (bs <> bs2)
        | otherwise -> pure { before, after }

    getLen = do
      bs <- readSource src
      if S.null bs
        then throwError (error ("Assertion Failed"))
        else do
          { before, after } <- mayBreak bs
          let
            w      = S.foldl (\i c -> i * 16 + (hexToWord c)) 0 $ S.takeWhile isHexDigit before
            after' = S.drop 1 after
          after'' <- if S.null after' then readSource src else pure after'
          withLen w after''

hexToWord :: Int -> Int
hexToWord w
  | w < 58    = w - 48
  | w < 71    = w - 55
  | otherwise = w - 87

isHexDigit :: Int -> Boolean
isHexDigit w = w >= 48 && w <= 57
            || w >= 65 && w <= 70
            || w >= 97 && w <= 102
