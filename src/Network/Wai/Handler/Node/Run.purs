module Network.Wai.Handler.Node.Run where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Aff (Aff, bracket, makeAff, runAff_)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref as Ref
import Control.Monad.Error.Class (catchError)
import Control.Parallel.Class (parallel, sequential)

import Data.ByteString as B
import Data.ByteString.Node.Buffer (toBuffer)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Posix.Signal (Signal(SIGTERM, SIGINT))

import Node.HTTP as N
import Node.Process as Proc

import Network.Wai (Application, defaultRequest)
import Network.Wai.Handler.Node.Buffer (newBufferPool, allocateBuffer)
import Network.Wai.Handler.Node.Date as D
import Network.Wai.Handler.Node.Effects (WaiEffects)
import Network.Wai.Handler.Node.Types as Z
import Network.Wai.Handler.Node.FdCache as F
import Network.Wai.Handler.Node.FileInfoCache as I
import Network.Wai.Handler.Node.Timeout as T
import Network.Wai.Handler.Node.Header (defaultIxReqHdrs)
import Network.Wai.Handler.Node.Recv (receiveBuf, receive)
import Network.Wai.Handler.Node.Request (recvRequest)
import Network.Wai.Handler.Node.Response (sendResponse)
import Network.Wai.Handler.Node.SendFile (sendFile)
import Network.Wai.Handler.Node.Utils (writeRawHTTP, endRawHTTP, getHttpSocket, closeHTTPServer)


runDefault :: forall eff. Z.SocketOption -> Application (WaiEffects eff) -> Aff (WaiEffects eff) Unit
runDefault sock app = runSettingsServer (Z.Settings (defSett { socketOption = sock })) app
  where
  defSett = case Z.defaultSettings of Z.Settings sett -> sett

runSettingsServer
  :: forall eff
   . Z.Settings (WaiEffects eff)
  -> Application (WaiEffects eff)
  -> Aff (WaiEffects eff) Unit
runSettingsServer sett@(Z.Settings set) app = withII0 $ runServer sett app
  where
  withII0 action =
    withTimeoutManager \tm ->
      D.withDateCache \ud ->
        F.withFdCache fdCacheDuration \fdc ->
          I.withFileInfoCache fileInfoCacheDuration \fic ->
            let ii = Z.InternalInfo0 tm ud fdc fic
            in action ii
  fdCacheDuration = set.fdCacheDuration * 1000.00
  fileInfoCacheDuration = set.fileInfoCacheDuration * 1000.00
  timeoutDuration = set.timeout * 1000.00
  withTimeoutManager f = case set.manager of
    Just tm -> f tm
    Nothing ->
      bracket
        (T.initialize timeoutDuration)
        T.stopManager
        f

runServer
  :: forall eff
   . Z.Settings (WaiEffects eff)
  -> Application (WaiEffects eff)
  -> Z.InternalInfo0 (WaiEffects eff)
  -> Aff (WaiEffects eff) Unit
runServer sett@(Z.Settings set) app ii0 =
  bracket
    (liftEff (N.createServer handleRequest))
    shutdownServer
    (\serv -> connect serv *> trapSignal)
  where
  connect serv = makeAff \cb -> case set.socketOption of
    Z.SockTCP hostname port backlog -> do
      _ <- N.listen serv { hostname, port, backlog } (cb (Right unit))
      pure mempty
    Z.SockUnix pathname -> do
      _ <- N.listenSocket serv pathname (cb (Right unit))
      pure mempty

  handleRequest :: N.Request -> N.Response -> Eff (WaiEffects eff) Unit
  handleRequest req res = runAff_ errHd (handleRequest' req res)
    where
    errHd = case _ of
      Left e  -> set.onException Nothing e
      Right _ -> pure unit

  handleRequest' :: N.Request -> N.Response -> Aff (WaiEffects eff) Unit
  handleRequest' req res = do
    void $ withClosedRef \ref ->
      bracket
        (liftEff $ httpConnection req res)
        (cleanup ref)
        (serve ref)
    where
    serve ref conn = bracket register cancel \th ->
      let ii1 = Z.toInternalInfo th ii0
      in serveConnection req conn ii1 sett app
      where
      register = T.register (Z.timeoutManager0 ii0) (cleanup ref conn)
      cancel   = T.tickle

    cleanup ref (Z.Connection { connClose }) = do
      isClosed <- liftEff $ Ref.modifyRef' ref (\x -> { state: true, value: x })
      unless isClosed $ connClose

    withClosedRef inner = liftEff (Ref.newRef false) >>= inner

serveConnection
  :: forall eff
   . N.Request
  -> Z.Connection (WaiEffects eff)
  -> Z.InternalInfo (WaiEffects eff)
  -> Z.Settings (WaiEffects eff)
  -> Application (WaiEffects eff)
  -> Aff (WaiEffects eff) Unit
serveConnection req conn ii sets@(Z.Settings set) app = do
  istatusRef <- liftEff $ Ref.newRef false
  recv0 <- wrappedRecv conn th istatusRef set.slowlorisSize
  _ <- liftEff $ Ref.writeRef istatusRef true
  { request, ixhdrs, rbody } <- recvRequest sets conn ii req recv0
  processRequest istatusRef request ixhdrs rbody `catchError` \e -> do
    _ <- sendErrorResponse istatusRef e
    liftEff $ set.onException (Just request) e
  where
  sendErrorResponse isStatus err = do
    status <- liftEff $ Ref.readRef isStatus
    when status do
      sendResponse sets conn ii defaultRequest defaultIxReqHdrs (pure B.empty) (set.onExceptionResponse err)
  processRequest istatus request ixhdrs recv = do
    _ <- T.pause th
    app request \response -> do
      _ <- T.resume th
      sendResponse sets conn ii request ixhdrs recv response
  th = Z.timeoutHandle ii

httpConnection :: forall eff. N.Request -> N.Response -> Eff (WaiEffects eff) (Z.Connection (WaiEffects eff))
httpConnection req resp = do
  bufferPool <- newBufferPool
  writeBuf <- allocateBuffer 16384
  sock <- getHttpSocket resp
  let
    write bs = do
      buf <- liftEff (toBuffer bs)
      writeRawHTTP resp buf
  pure $ Z.Connection
    { connSendMany: traverse_ write
    , connSendAll:  write
    , connSendFile: sendFile sock
    , connClose: endRawHTTP resp
    , connRecvBuf: receiveBuf (N.requestAsStream req)
    , connRecv: receive (N.requestAsStream req) bufferPool
    , connWriteBuffer: writeBuf
    , connBufferSize: 16384
    }

shutdownServer :: forall eff. N.Server -> Aff (WaiEffects eff) Unit
shutdownServer serv = do
  _ <- closeHTTPServer serv
  liftEff $ Proc.exit 0

trapSignal :: forall eff. Aff (WaiEffects eff) Unit
trapSignal = sequential $ parallel sigterm <|> parallel sigint
  where
  sigterm = makeAff \cb -> do
    _ <- Proc.onSignal SIGTERM (cb $ Right unit)
    pure mempty
  sigint  = makeAff \cb -> do
    _ <- Proc.onSignal SIGINT (cb $ Right unit)
    pure mempty

wrappedRecv
  :: forall eff
   . Z.Connection (WaiEffects eff)
  -> T.Handle (WaiEffects eff)
  -> Ref.Ref Boolean
  -> Int
  -> Aff (WaiEffects eff) (Z.Recv (WaiEffects eff))
wrappedRecv (Z.Connection { connRecv }) th istatus slowlorisSize = pure $ do
  bs <- connRecv
  unless (B.null bs) do
    liftEff $ Ref.writeRef istatus true
    when (B.length bs >= slowlorisSize) $ T.tickle th
  pure bs
