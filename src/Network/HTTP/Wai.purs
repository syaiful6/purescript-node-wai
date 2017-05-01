module Network.HTTP.Wai
  ( Application
  , Middleware
  , toWaiRequest
  , reqPathInfo
  , reqHeaders
  , reqHttpVersion
  , reqQuery
  , reqBody
  , reqMethod
  , responseFile
  , responseBuffer
  , responseStream
  , responseStrUtf8
  , responseStatus
  , responseHeaders
  , modifyResponse
  , ifRequest
  , module H
  , module Network.HTTP.Wai.Internal
  ) where

import Prelude

import Data.Either (either)
import Data.List (List(..), (:), fromFoldable)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as S
import Data.StrMap as SM
import Data.Tuple (Tuple)
import Data.URI.Query (Query, parseQuery)

import Control.Monad.Eff (Eff)

import Node.Buffer (Buffer)
import Node.Encoding (Encoding(UTF8))
import Node.HTTP as NH
import Node.Stream (Readable)

import Text.Parsing.StringParser (runParser)

import Network.HTTP.Types as H
import Network.HTTP.Wai.Internal (Request(..), Response(..), FilePath, FilePart(..), ResponseReceived(..))


type Application e =
  Request -> (Response e -> Eff e ResponseReceived) -> Eff e ResponseReceived

type Middleware e = Application e -> Application e

reqPathInfo :: Request -> List String
reqPathInfo (Request s) = s.pathInfo

reqHeaders :: Request -> H.RequestHeaders
reqHeaders (Request s) = s.headers

reqHttpVersion :: Request -> H.HttpVersion
reqHttpVersion (Request s) = s.httpVersion

reqQuery :: Request -> Maybe Query
reqQuery (Request s) = s.query

reqMethod :: Request -> H.Method
reqMethod (Request s) = s.method

reqBody :: forall e. Request -> Readable () (http :: NH.HTTP | e)
reqBody (Request s) = s.body

responseFile :: forall e. H.Status -> H.ResponseHeaders -> FilePath -> Maybe FilePart -> Response e
responseFile = ResponseFile

responseBuffer :: forall e. H.Status -> H.ResponseHeaders -> Buffer -> Response e
responseBuffer = ResponseBuffer

responseString :: forall e.  H.Status -> H.ResponseHeaders -> Encoding -> String -> Response e
responseString = ResponseString

responseStream
  :: forall e
   . H.Status
  -> H.ResponseHeaders
  -> (forall a. Readable a e)
  -> Response e
responseStream = ResponseStream

responseStrUtf8 :: forall e. H.Status -> H.ResponseHeaders -> String -> Response e
responseStrUtf8 s h = responseString s h UTF8

responseStatus :: forall e. Response e -> H.Status
responseStatus (ResponseFile s _ _ _)   = s
responseStatus (ResponseStream s _ _)   = s
responseStatus (ResponseBuffer s _ _)   = s
responseStatus (ResponseString s _ _ _) = s

responseHeaders :: forall e. Response e -> H.ResponseHeaders
responseHeaders (ResponseFile _ h _ _)   = h
responseHeaders (ResponseStream _ h _)   = h
responseHeaders (ResponseBuffer _ h _)   = h
responseHeaders (ResponseString _ h _ _) = h

modifyResponse :: forall eff. (Response eff -> Response eff) -> Middleware eff
modifyResponse f app req respond = app req $ respond <<< f

ifRequest :: forall eff. (Request -> Boolean) -> Middleware eff -> Middleware eff
ifRequest rpred middle app req = if rpred req then middle app req else app req

parsedQuery :: String -> Maybe (Query)
parsedQuery = either (const Nothing) Just <<< runParser parseQuery

pathSegments :: String -> List String
pathSegments ""  = Nil
pathSegments "/" = Nil
pathSegments s   = normalizePath $ fromFoldable (S.split (S.Pattern "/") s)
  where
    normalizePath ("":xs) = xs
    normalizePath xs = xs

toWaiRequest :: NH.Request -> Request
toWaiRequest req =
  let
    rawPathInfo = NH.requestURL req
    idxparam = S.indexOf (S.Pattern "?") rawPathInfo
    rawQs = flip S.drop rawPathInfo <<< (+) 1 <$> idxparam
    pathInfo = pathSegments $ fromMaybe rawPathInfo (flip S.take rawPathInfo <$> idxparam)
    headers = SM.toUnfoldable (NH.requestHeaders req) :: Array (Tuple String String)
  in
    Request $
      { httpVersion: fromMaybe H.http10 (H.string2HttpVersion $ NH.httpVersion req)
      , method: fromMaybe H.GET (H.string2HTTPMethod $ NH.requestMethod req)
      , rawPathInfo: rawPathInfo
      , rawQueryString: fromMaybe "" rawQs
      , query: rawQs >>= parsedQuery
      , pathInfo: pathInfo
      , headers: H.fromHeaders headers
      , body: NH.requestAsStream req
      }
