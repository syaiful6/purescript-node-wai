module Test.Network.Wai.Handler.Node.ResponseHeader (mainResponseHeaderSpec) where

import Prelude

import Control.Monad.Eff.Class (liftEff)

import Data.Tuple (Tuple(..))
import Data.List (List(Nil), (:))
import Data.ByteString (ByteString, pack)
import Data.String (toCharArray) as S
import Data.Char (toCharCode) as S

import Test.Unit (TestSuite, describe, it)
import Test.Unit.Assert (shouldEqual)

import Network.Wai.Types as H
import Network.Wai.Handler.Node.ResponseHeader (composeHeader)

composedHeader :: ByteString
composedHeader = pack (S.toCharCode <$> sChars)
  where
  sChars = S.toCharArray $
    "HTTP/1.1 200 OK\r\nDate: Mon, 13 Aug 2012 04:22:55 GMT\r\nContent-Length: 151\r\nServer: Mighttpd/2.5.8\r\nLast-Modified: Fri, 22 Jun 2012 01:18:08 GMT\r\nContent-Type: text/html\r\n\r\n"

headers :: H.ResponseHeaders
headers = (
    Tuple H.hDate "Mon, 13 Aug 2012 04:22:55 GMT"
  : Tuple H.hContentLength "151"
  : Tuple H.hServer "Mighttpd/2.5.8"
  : Tuple H.hLastModified "Fri, 22 Jun 2012 01:18:08 GMT"
  : Tuple H.hContentType "text/html"
  : Nil
  )

mainResponseHeaderSpec :: forall eff. TestSuite eff
mainResponseHeaderSpec = do
  describe "Response Header" do
    it "composes a HTTP header" do
      com <- liftEff $ composeHeader H.http11 H.ok200 headers
      com `shouldEqual` composedHeader
