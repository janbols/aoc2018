module Main where

import Data.Char
import Utils
import Control.Applicative

main :: IO ()
main = do
  ptNr <- partSelection
  input <- requestMultilineInput
  if ptNr == 1 then print $ pt1 input
    else print $ pt2 input


pt1 :: String -> Int
pt1 input =
  let ss = lines input
      nrOf2 = length $ filter (containsExactlyXChars 2)  ss
      nrOf3 = length $ filter (containsExactlyXChars 3)  ss
  in nrOf2 * nrOf3


containsExactlyXChars :: Int -> String -> Bool
containsExactlyXChars cnt s =
  let alphabet = ['a' .. 'z']
      counts = (count <$> alphabet) <*> [s]
   in cnt `elem` counts

count :: Eq a => a -> [a] -> Int
count x = length . filter (x==)


pt2 :: String -> String
pt2 input =
  let ss = lines input
      (x, y) = head [(x, y) | x <- ss, y <- ss, x /= y, length (differentChars x y) == 1]
      common = commonChars x y
   in map fst common


differentChars :: String -> String -> [(Char,Char)]
differentChars  s1 s2 =
  let zipped = zip s1 s2
  in filter (uncurry (/=)) zipped

commonChars :: String -> String -> [(Char,Char)]
commonChars s1 s2 =
  let zipped = zip s1 s2
  in filter (uncurry (==)) zipped



