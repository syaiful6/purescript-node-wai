module Network.HTTP.Types.Version where

import Prelude

import Data.Array as A
import Data.Maybe (Maybe(..))
import Data.Int (fromString)
import Data.String as S

-- | HTTP Version, the first component is major and the second is minor
data HttpVersion = HttpVersion Int Int

derive instance eqHttpVersion :: Eq HttpVersion
derive instance ordHttpVersion :: Ord HttpVersion

instance showHttpVersion :: Show HttpVersion where
  show (HttpVersion major minor) = "HTTP/" <> show major <> "." <> show minor

http09 :: HttpVersion
http09 = HttpVersion 0 9

http10 :: HttpVersion
http10 = HttpVersion 1 0

http11 :: HttpVersion
http11 = HttpVersion 1 1

string2HttpVersion :: String -> Maybe HttpVersion
string2HttpVersion s = parsed $ S.split (S.Pattern ".") s
  where
    parsed pair =
      if A.length pair == 2
        then do
          major <- fromString =<< A.index pair 0
          minor <- fromString =<< A.index pair 1
          if (major >= 0) && (minor >= 0)
            then pure (HttpVersion major minor)
            else Nothing
      else Nothing
