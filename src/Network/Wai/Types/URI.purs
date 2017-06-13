module Network.Wai.Types.URI where

import Prelude

import Data.List (List(Nil), (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.String as S

type QueryItem = Tuple String (Maybe String)

type Query = List QueryItem

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
    | S.null qs = xs
    | otherwise =
        let Tuple y ys  = breakDiscard "&;" qs
            parsePair p =
              let Tuple k v = break (_ `eq` '=') p
                  v'' = case S.uncons v
        in parseQuery' (parsePair y : xs) ys

breakDiscard :: String -> String -> Tuple String String
breakDiscard seps s =
  let { before, after } = break ((_ `S.contains` seps) <<< S.Pattern <<< S.singleton)
  in Tuple before (S.drop 1 y)

break :: (Char -> Boolean) -> String -> { before :: String, after :: String }
break k ps = case findIndexOrEnd k ps of n -> { before: S.take n ps, after: S.drop n ps }

findIndexOrEnd :: (Char -> Boolean) -> String -> Int
findIndexOrEnd k x = go 0
  where
  go n
    | n >= S.length x = (S.length x) - 1
    | otherwise       = case S.charAt n x of
        Nothing -> (S.length x) - 1
        Just w  -> if k w then n else go (n + 1)
