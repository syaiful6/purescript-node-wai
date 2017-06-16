module Network.Wai.Types.URI
  ( QueryItem
  , Query
  , parseQuery
  , pathSegments
  ) where

import Prelude

import Data.Either (fromRight)
import Data.List (List(Nil), (:), reverse, fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.String as S
import Data.String.Regex as RX
import Data.String.Regex.Flags as RXF

import Global (encodeURIComponent, decodeURIComponent)

import Partial.Unsafe (unsafePartial)


type QueryItem = Tuple String (Maybe String)

type Query = List QueryItem

pathSegments :: String -> List String
pathSegments ""  = Nil
pathSegments "/" = Nil
pathSegments s   = normalizePath $ fromFoldable (S.split (S.Pattern "/") s)
  where
  normalizePath ("":xs) = xs
  normalizePath xs = xs

parseQuery :: String -> Query
parseQuery = parseQuery' Nil <<< dropQuestion
  where
  dropQuestion :: String -> String
  dropQuestion s = case S.uncons s of
    Nothing -> ""
    Just { head, tail }
      | head == '?' -> tail
      | otherwise   -> s

  parseQuery' :: Query -> String -> Query
  parseQuery' xs qs
    | S.null qs = reverse xs
    | otherwise =
        let Tuple y ys  = breakDiscard "&;" qs
            parsePair p =
              let { before, after } = break (_ `eq` '=') p
                  v'' = case S.uncons after of
                    Nothing       -> Nothing
                    Just { tail } -> Just $ decodeURI tail
              in Tuple (decodeURI before) v''
        in parseQuery' (parsePair y : xs) ys

breakDiscard :: String -> String -> Tuple String String
breakDiscard seps s =
  let { before, after } = break ((_ `S.contains` seps) <<< S.Pattern <<< S.singleton) s
  in Tuple before (S.drop 1 after)

break :: (Char -> Boolean) -> String -> { before :: String, after :: String }
break k ps = case findIndexOrEnd k ps of n -> { before: S.take n ps, after: S.drop n ps }

findIndexOrEnd :: (Char -> Boolean) -> String -> Int
findIndexOrEnd k x = go 0
  where
  go n
    | n >= S.length x = S.length x
    | otherwise       = case S.charAt n x of
        Nothing -> S.length x
        Just w  -> if k w then n else go (n + 1)

encodeURI :: String -> String
encodeURI = RX.replace rgxSpace "+" <<< encodeURIComponent

decodeURI :: String -> String
decodeURI = decodeURIComponent <<< RX.replace rgxPlus " "

rgxSpace :: RX.Regex
rgxSpace = unsafePartial $ fromRight $ RX.regex "%20" RXF.global

rgxPlus :: RX.Regex
rgxPlus = unsafePartial $ fromRight $ RX.regex "\\+" RXF.global
