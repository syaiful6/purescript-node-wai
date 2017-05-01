module Network.HTTP.Types.Method
  ( Method(..)
  , string2HTTPMethod
  )where

import Prelude

import Data.Maybe (Maybe(..))

data Method
  = DELETE
  | GET
  | HEAD
  | OPTIONS
  | PATCH
  | POST
  | PUT

derive instance eqMethod :: Eq Method
derive instance ordMethod :: Ord Method

instance showMethod :: Show Method where
  show DELETE = "DELETE"
  show GET = "GET"
  show HEAD = "HEAD"
  show OPTIONS = "OPTIONS"
  show PATCH = "PATCH"
  show POST = "POST"
  show PUT = "PUT"

string2HTTPMethod :: String -> Maybe Method
string2HTTPMethod "DELETE" = Just DELETE
string2HTTPMethod "GET" = Just GET
string2HTTPMethod "HEAD" = Just HEAD
string2HTTPMethod "OPTIONS" = Just OPTIONS
string2HTTPMethod "PATCH" = Just PATCH
string2HTTPMethod "POST" = Just POST
string2HTTPMethod "PUT" = Just PUT
string2HTTPMethod _ = Nothing
