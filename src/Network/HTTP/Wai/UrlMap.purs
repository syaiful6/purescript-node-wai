module Network.HTTP.Wai.UrlMap where

import Prelude

import Data.Foldable (foldl, intercalate)
import Data.List (List(Nil), (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (class Monoid, mempty)
import Data.URI.Query (printQuery)
import Data.Tuple (Tuple(..))

import Network.HTTP.Types (status404, contentType)
import Network.HTTP.Wai (Application, Request(..), reqPathInfo, responseStrUtf8)

type Path = List String

newtype UrlMap eff = UrlMap (List (Tuple Path (Application eff)))

instance semigroupUrlMap :: Semigroup (UrlMap eff) where
  append (UrlMap a) (UrlMap b) = UrlMap (a <> b)

instance monoidUrlMap :: Monoid (UrlMap eff) where
  mempty = UrlMap Nil

mount' :: forall eff. Path -> Application eff -> UrlMap eff
mount' prefix thing = UrlMap $ (Tuple prefix thing : Nil)

mount :: forall eff. String -> Application eff -> UrlMap eff
mount prefix = mount' (prefix:Nil)

mountRoot :: forall eff. Application eff -> UrlMap eff
mountRoot = mount' Nil

match :: forall a. Path -> List (Tuple Path a) -> Maybe (Tuple Path a)
match xs tuples = foldl go Nothing tuples
  where
    go (Just x) _ = Just x
    go _ (Tuple prefix y) = stripPrefix prefix xs >>= \xs' -> pure (Tuple xs' y)

toApplication :: forall eff. UrlMap eff -> Application eff
toApplication (UrlMap urlmap) req sendResponse =
  case match (reqPathInfo req) urlmap of
    Just (Tuple newPath app) ->
      app (modifyReqPath newPath req) sendResponse
    Nothing ->
      sendResponse $ responseStrUtf8 status404 ([contentType "text/plain"]) "Not found\n"

modifyReqPath :: Path -> Request -> Request
modifyReqPath pa (Request oreq) =
  let
    newRaw = "/" <> intercalate "/" pa <> fromMaybe mempty (printQuery <$> oreq.query)
  in
    Request $
      oreq { rawPathInfo = newRaw
           , pathInfo = pa }

stripPrefix :: forall a. Eq a => List a -> List a -> Maybe (List a)
stripPrefix Nil ys = Just ys
stripPrefix (x:xs) (y:ys)
  | x == y = stripPrefix xs ys
stripPrefix _ _ = Nothing
