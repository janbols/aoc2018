module Main where

import Data.Char
import Utils

main :: IO ()
main = do
  ptNr <- partSelection
  input <- requestInput
  if ptNr == 1 then print $ pt1 input
    else print $ pt2 input


pt1 :: String -> Int
pt1 = inverseCaptcha 1

pt2 :: String -> Int
pt2 cs = inverseCaptcha (length cs `quot` 2) cs

inverseCaptcha :: Int -> String -> Int
inverseCaptcha dropCnt cs =
  let xs = withPeek dropCnt (map digitToInt cs)
  in xs -: filter pairMatches -: map fst -: sum

withPeek:: Int -> [a]-> [(a,a)]
withPeek dropCnt xs = zip xs $ drop dropCnt $ cycle xs

pairMatches :: Eq a => (a,a) -> Bool
pairMatches = uncurry (==)

(-:) :: a -> ( a -> b ) -> b
x -: f = f x
