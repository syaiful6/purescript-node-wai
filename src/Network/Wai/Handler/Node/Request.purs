module Network.Wai.Handler.Node.Request (recvRequest) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref as Ref

import Data.ByteString as B
import Data.String as S
import Data.List (List(Nil))
import Data.Maybe (fromMaybe)
import Data.StrMap as SM
import Data.Tuple (Tuple)

import Node.HTTP as N
import Unsafe.Coerce (unsafeCoerce)

import Network.Wai (Request(..))
import Network.Wai.Types as H
import Network.Wai.Handler.Node.Types as Z
import Network.Wai.Handler.Node.Timeout as T
import Network.Wai.Handler.Node.Header (IndexedHeader, indexRequestHeader)
import Network.Wai.Handler.Node.Effects (WaiEffects)


-- Create WAI request from node request
recvRequest
  :: forall eff
   . Z.Settings (WaiEffects eff)
  -> Z.Connection (WaiEffects eff)
  -> Z.InternalInfo (WaiEffects eff)
  -> N.Request
  -> Z.Recv (WaiEffects eff)
  -> Aff
      (WaiEffects eff)
      ({ request :: Request (WaiEffects eff)
      , ixhdrs :: IndexedHeader
      , rbody :: Z.Recv (WaiEffects eff) })
recvRequest sets conn ii req recv = do
  rbody <- timeoutBody th recv
  let
    request = Request
      { method: httpMethod
      , headers: reqHeaders
      , httpVersion: fromMaybe H.http10 $ H.string2HttpVersion (N.httpVersion req)
      , rawPathInfo: rawPathInfo
      , rawQueryString: fromMaybe "" rawQs
      , query: fromMaybe Nil (H.parseQuery <$> rawQs)
      , pathInfo: pathInfo
      , body: rbody
      }
  pure $
    { request
    , ixhdrs: indexRequestHeader reqHeaders
    , rbody: rbody
    }
  where
  th          = Z.timeoutHandle ii
  rawPathInfo = N.requestURL req
  idxparam = S.indexOf (S.Pattern "?") rawPathInfo
  rawQs = flip S.drop rawPathInfo <<< (+) 1 <$> idxparam
  pathInfo = H.pathSegments $ fromMaybe rawPathInfo (flip S.take rawPathInfo <$> idxparam)
  reqHeaders :: H.RequestHeaders
  reqHeaders = unsafeCoerce $ (SM.toUnfoldable (N.requestHeaders req) :: List (Tuple String String))
  httpMethod :: H.Method
  httpMethod = fromMaybe H.GET $ H.string2HTTPMethod (N.requestMethod req)

timeoutBody
  :: forall eff
   . T.Handle (WaiEffects eff)
  -> Z.Recv (WaiEffects eff)
  -> Aff (WaiEffects eff) (Z.Recv (WaiEffects eff))
timeoutBody th recv = do
  isFirstRef <- liftEff $ Ref.newRef true
  pure $ do
    isFirst <- liftEff $ Ref.readRef isFirstRef
    when isFirst $ do
      T.resume th
      liftEff $ Ref.writeRef isFirstRef false
    bs <- recv
    when (B.null bs) (T.pause th)
    pure bs
