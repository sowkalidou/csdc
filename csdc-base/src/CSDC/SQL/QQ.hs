{-# LANGUAGE TemplateHaskell #-}

module CSDC.SQL.QQ
  ( sqlqq,
  )
where

import Data.Char
import Data.List hiding (head, tail)
import Data.Maybe
import GHC.Exts (IsString (..))
import Language.Haskell.TH.Quote
import Prelude hiding (head, tail)

sqlqq :: QuasiQuoter
sqlqq =
  QuasiQuoter
    ((\a -> [|fromString a|]) . trim . unindent)
    (error "Cannot use q as a pattern")
    (error "Cannot use q as a type")
    (error "Cannot use q as a dec")

unindent :: String -> String
unindent s =
  case lines s of
    head : tail ->
      let unindentedHead = dropWhile (== ' ') head
          minimumTailIndent = minimumIndent . unlines $ tail
          unindentedTail = case minimumTailIndent of
            Just indent -> map (drop indent) tail
            Nothing -> tail
       in unlines $ unindentedHead : unindentedTail
    [] -> []

minimumIndent :: String -> Maybe Int
minimumIndent =
  listToMaybe
    . sort
    . map lineIndent
    . filter (not . null . dropWhile isSpace)
    . lines

-- | Amount of preceding spaces on first line
lineIndent :: String -> Int
lineIndent = length . takeWhile (== ' ')

trim :: String -> String
trim = dropWhileRev isSpace . dropWhile isSpace

dropWhileRev :: (a -> Bool) -> [a] -> [a]
dropWhileRev p = foldr (\x xs -> if p x && null xs then [] else x : xs) []
