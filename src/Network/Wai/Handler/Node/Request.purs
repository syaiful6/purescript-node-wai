module Network.Wai.Handler.Node.Request where

import Prelude

import Control.Monad.Aff (Aff, delay)
import Control.Monad.Aff.AVar as AV
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (Ref, newRef, modifyRef, writeRef, readRef)

import Data.ByteString as B
import Data.ByteString.Node.Stream (Readable, onData, onEnd, onError)
import Data.CatQueue as Q
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Tuple (Tuple(..))

import Network.Wai.Handler.Node.Utils (withAVar, handleAff)
import Network.Wai.Handler.Node.Effects (WaiEffects)


data Entry a
  = Entry a
  | EOF

data BodySource =
  BodySource
    (AV.AVar Unit)
    (Ref Boolean)
    (Ref (Q.CatQueue (Entry B.ByteString)))

mkBodySource :: forall eff. Aff (WaiEffects eff) BodySource
mkBodySource = do
  v <- AV.makeVar' unit
  ref <- liftEff $ newRef Q.empty
  csm <- liftEff $ newRef false
  pure $ BodySource v csm ref

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
readBody s (BodySource lock started ref) = do
  eof     <- liftEff $ newRef false
  pure $ do
    isStarted <- liftEff $ readRef started
    if isStarted
      then read eof
      else do
        _ <- liftEff do
          _ <- start eof
          writeRef started true
        read eof
  where
  start eof = do
    _ <- onData s (handleAff <<< handleData)
    _ <- onEnd s (handleAff handleEnd)
    onError s (handleAff <<< handleError eof)

  read eof = withAVar lock \_ -> do
    isEof <- liftEff $ readRef eof
    if isEof
      then pure B.empty
      else do
        v <- liftEff $ readRef ref
        case Q.uncons v of
          Nothing -> do
            _ <- delay (wrap 1000.00)
            read eof
          Just (Tuple (Entry a) qs) -> do
            _ <- liftEff $ writeRef ref qs
            pure a
          Just (Tuple EOF _) -> do
            _ <- liftEff $ writeRef ref Q.empty
            pure B.empty

  handleError eof err = withAVar lock \_ ->
    liftEff $ writeRef eof true

  handleEnd = withAVar lock \_ ->
    liftEff (modifyRef ref (\sq -> Q.snoc sq EOF))

  handleData bs = withAVar lock \_ ->
    if B.null bs
      then pure unit
      else liftEff (modifyRef ref (\sq -> Q.snoc sq (Entry bs)))
