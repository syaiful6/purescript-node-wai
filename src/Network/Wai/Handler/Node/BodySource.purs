module Network.Wai.Handler.Node.BodySource where

import Prelude

import Control.Monad.Aff (Aff, delay)
import Control.Monad.Aff.AVar as AV
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (newRef, writeRef, readRef)
import Control.Monad.Error.Class (throwError)
import Control.Monad.STM as S

import Data.ByteString as B
import Data.ByteString.Node.Stream (Readable, onData, onEnd, onError, pause, resume, isPaused)
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (wrap)

import Network.Wai.Handler.Node.Utils (handleAff, nextTickEff)
import Network.Wai.Handler.Node.Effects (WaiEffects)

import Unsafe.Coerce (unsafeCoerce)

data Entry a
  = Entry a
  | EOF

data BodySource = BodySource (S.TVar Int) (S.TQueue (Entry B.ByteString))

mkBodySource' :: forall eff. Int -> Aff (WaiEffects eff) BodySource
mkBodySource' size = BodySource <$> S.newTVarAff size <*> S.newTQueueAff

mkBodySource :: forall eff. Aff (WaiEffects eff) BodySource
mkBodySource = mkBodySource' (16 * 1024)

recvBody :: BodySource -> Entry B.ByteString -> S.STM Boolean
recvBody (BodySource _ queue) EOF = S.writeTQueue queue EOF $> true
recvBody (BodySource size queue) er@(Entry bs) = do
  size' <- S.readTVar size
  let i = size' - (B.length bs)
  _ <- S.writeTVar size i
  _ <- S.writeTQueue queue er
  pure $ if i <= 0 then true else false

isBodySourceFull :: BodySource -> S.STM Boolean
isBodySourceFull (BodySource size _) = do
  v <- S.readTVar size
  pure $ if v <= 0 then true else false

readBodySource :: BodySource -> S.STM (Entry B.ByteString)
readBodySource (BodySource size queue) = do
  i <- S.readTQueue queue
  case i of
    EOF -> pure i
    Entry bs -> do
      let len = B.length bs
      _ <- S.modifyTVar size ((+) len)
      pure i

flushBody :: forall eff. Aff (WaiEffects eff) B.ByteString -> Aff (WaiEffects eff) Unit
flushBody src = go
  where
  go = do
    bs <- src
    if B.null bs
      then pure unit
      else do
        delay (wrap 0.00)
        go

readBody
  :: forall w eff
   . Readable w (WaiEffects eff)
  -> BodySource
  -> Aff (WaiEffects eff) (Aff (WaiEffects eff) B.ByteString)
readBody s bso = do
  eof      <- liftEff $ newRef false
  errorRef <- liftEff $ newRef Nothing
  started <- liftEff $ newRef false
  pure $ do
    isStarted <- liftEff $ readRef started
    isFull <- S.atomically $ isBodySourceFull bso
    when (not isStarted && not isFull) $ liftEff $ start started errorRef
    recvB errorRef eof
  where
  start startRef errorRef = do
    _ <- onData s (handleAff <<< handleData startRef)
    _ <- onEnd s (handleAff handleEnd)
    _ <- onError s (handleError errorRef)
    p <- isPaused s
    _ <- when p (resume s)
    writeRef startRef true

  recvB errorRef eof = do
    me <- liftEff $ readRef errorRef
    case me of
      Just err -> do
        _ <- liftEff $ removeAllListener s
        throwError err
      Nothing -> do
        isEof <- liftEff $ readRef eof
        if isEof
          then pure B.empty
          else do
            v <- S.atomically (readBodySource bso)
            case v of
              Entry a -> pure a
              EOF -> do
                _ <- liftEff $ do
                  _ <- writeRef eof true
                  -- remove our listeners
                  removeAllListener s
                pure B.empty

  handleError errorRef err = writeRef errorRef (Just err)

  handleEnd = void $ S.atomically (recvBody bso EOF)

  handleData started bs =
    if B.null bs
      then pure unit
      else do
        full <- S.atomically (recvBody bso (Entry bs))
        when full $ liftEff do
          _ <- removeAllListener s
          -- it look like the stream can't be paused by simple using `pause` function.
          -- It maybe just work when pipe the stream to writable stream. So, we just
          -- remove all listeners to make the stream in pause mode
          writeRef started false

foreign import removeAllListener
  :: forall w eff. Readable w eff -> Eff eff Unit
