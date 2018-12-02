module Main where

import Data.Char
import Utils

main :: IO ()
main = do
  ptNr <- partSelection
  input <- requestMultilineInput
  if ptNr == 1 then print $ pt1 input
    else print $ pt2 input


pt1 :: String -> Int
pt1 input =
  let is = parseToListOfInts input
  in sum is



parseToListOfInts :: String -> [Int]
parseToListOfInts s =
  let ss = lines s
  in map parseSingleNumber ss

parseSingleNumber :: String -> Int
parseSingleNumber ('+':cs) = read cs
parseSingleNumber ('-':cs) = -1 * read cs
parseSingleNumber _ = 0


pt2 :: String -> Int
pt2 input =
  let is = cycle $ parseToListOfInts input
      freqs = scanl (+) 0 is
      scannedFreqs  = scanl (flip (:)) [] freqs
      correctResults = dropWhile (not . headInList) scannedFreqs
--  in head $ head correctResults
  in case correctResults of
    ((a:as):aas) -> a
    _ -> 0

headInList :: Eq a => [a] -> Bool
headInList (a:as) = a `elem` as
headInList _ = False