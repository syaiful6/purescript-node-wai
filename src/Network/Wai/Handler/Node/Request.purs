module Network.Wai.Handler.Node.Request where

import Prelude

import Control.Monad.Aff (Aff, delay)
import Control.Monad.Aff.AVar as AV
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (newRef, writeRef, readRef)
import Control.Monad.Error.Class (throwError)
import Control.Monad.STM as S

import Data.ByteString as B
import Data.ByteString.Node.Stream (Readable, onData, onEnd, onError)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)

import Network.Wai.Handler.Node.Utils (withAVar, handleAff)
import Network.Wai.Handler.Node.Effects (WaiEffects)


data Entry a
  = Entry a
  | EOF

data BodySource = BodySource (S.TVar Int) (S.TVar Boolean) (S.TQueue (Entry B.ByteString))

mkBodySource :: forall eff. Int -> Aff (WaiEffects eff) BodySource
mkBodySource size = BodySource <$> S.newTVarAff size <*> S.newTVarAff false <*> S.newTQueueAff

recvBody :: BodySource -> Entry B.ByteString -> S.STM Unit
recvBody (BodySource _ _ queue) EOF = S.writeTQueue queue EOF
recvBody (BodySource size _ queue) er@(Entry bs) = do
  let len = B.length bs
  size' <- S.readTVar size
  if (size' - len) <= 0
    then S.retry
    else do
      _ <- S.writeTVar size (size' - len)
      S.writeTQueue queue er

readBodySource :: BodySource -> S.STM (Entry B.ByteString)
readBodySource (BodySource size _ queue) = do
  s <- S.readTVar size
  i <- S.readTQueue queue
  case i of
    EOF -> pure i
    Entry bs -> do
      let len = B.length bs
      S.writeTVar size (s + len)
      pure i

flushBody :: forall eff. Aff (WaiEffects eff) B.ByteString -> Aff (WaiEffects eff) Unit
flushBody src = go
  where
  go = do
    bs <- src
    if B.null bs
      then pure unit
      else do
        delay (wrap 100.00)
        go

readBody
  :: forall w eff
   . Readable w (WaiEffects eff)
  -> BodySource
  -> Aff (WaiEffects eff) (Aff (WaiEffects eff) B.ByteString)
readBody s bso@(BodySource _ started _) = do
  eof      <- liftEff $ newRef false
  errorRef <- liftEff $ newRef Nothing
  lock <- AV.makeVar' unit
  pure $ do
    isStarted <- S.atomically $ S.readTVar started
    if isStarted
      then read lock errorRef eof
      else do
        _ <- liftEff $ start lock errorRef
        _ <- S.atomically $ S.writeTVar started true
        read lock errorRef eof
  where
  start lock errorRef = do
    _ <- onData s (handleAff <<< handleData)
    _ <- onEnd s (handleAff handleEnd)
    onError s (handleAff <<< handleError lock errorRef)

  read lock errorRef eof = withAVar lock \_ -> do
    me <- liftEff $ readRef errorRef
    case me of
      Just err -> throwError err
      Nothing -> do
        isEof <- liftEff $ readRef eof
        if isEof
          then pure B.empty
          else do
            v <- S.atomically (readBodySource bso)
            case v of
              Entry a -> pure a
              EOF     -> do
                _ <- liftEff $ writeRef eof true
                pure B.empty

  handleError lock errorRef err = withAVar lock \_ -> liftEff $ writeRef errorRef (Just err)

  handleEnd = S.atomically (recvBody bso EOF)

  handleData bs =
    if B.null bs
      then pure unit
      else S.atomically (recvBody bso (Entry bs))
