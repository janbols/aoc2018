{-# LANGUAGE TupleSections #-}

module Main where

import           Data.List
import           Utils

main :: IO ()
main = do
  ptNr <- partSelection
  input <- requestMultilineInput
  if ptNr == 1
    then print $ pt1 input
    else print $ pt2 input

pt1 :: String -> Int
pt1 input =
  let points = map parsePoint $ lines input
      (left, top, right, bottom) = boundaries points
      totalArea = [(x, y) | x <- [left .. right], y <- [top .. bottom]]
      distancesForEachPoint = map (`distancesToRefPoint` totalArea) points  {-bad idea!!!-}
      emptyTotalArea = map (const []) totalArea
      totalAreaWithDistances = foldr (zipWith (:)) emptyTotalArea distancesForEachPoint
      totalAreaMinima = map (\ds -> map (\d -> if d == minimum ds then 1 else 0) ds)  totalAreaWithDistances
      totalAreaStrictMinima = filter ((1 ==) . sum) totalAreaMinima
      minAreaPerRefPoint = foldl1 (zipWith (+)) totalAreaStrictMinima
      minAreaWithRefPoint = zip points minAreaPerRefPoint
      finiteCandidates = filter (\(px, py) -> (px > left) && (px < right) && (py > top) && (py < bottom)) points
      validMinAreaWithRefPoint = filter (\(p, area) -> p `elem` finiteCandidates) minAreaWithRefPoint
      validMinArea = map snd validMinAreaWithRefPoint
   in maximum validMinArea

pt2 :: String -> Int
pt2 input =
  let points = map parsePoint $ lines input
      (left, top, right, bottom) = boundaries points
      area = [(x, y) | x <- [left .. right], y <- [top .. bottom]]
      areaDistancesToEachRef = map (`distancesToRefPoint` points) area
      areaDistanceSum = map sum areaDistancesToEachRef
  in length $ filter (<10000) areaDistanceSum

type Point = (Int, Int)

parsePoint :: String -> Point
parsePoint p =
  let ws = words p
   in (read $ init $ ws !! 0, read $ ws !! 1)

boundaries :: [Point] -> (Int, Int, Int, Int)
boundaries =
  foldl
    (\(left, top, right, bottom) (px, py) -> (min left px, min top py, max right px, max bottom py))
    (90000, 90000, 0, 0)

distance :: Point -> Point -> Int
distance (p1x, p1y) (p2x, p2y) = abs (p1x - p2x) + abs (p1y - p2y)

distancesToRefPoint :: Point -> [Point] -> [Int]
distancesToRefPoint ref = map (distance ref)

testPoints = map parsePoint testData

testData = ["1, 1", "1, 6", "8, 3", "3, 4", "5, 5", "8, 9"]

inputData =
  [ "80, 357"
  , "252, 184"
  , "187, 139"
  , "101, 247"
  , "332, 328"
  , "302, 60"
  , "196, 113"
  , "271, 201"
  , "334, 89"
  , "85, 139"
  , "327, 161"
  , "316, 352"
  , "343, 208"
  , "303, 325"
  , "316, 149"
  , "270, 319"
  , "318, 153"
  , "257, 332"
  , "306, 348"
  , "299, 358"
  , "172, 289"
  , "303, 349"
  , "271, 205"
  , "347, 296"
  , "220, 276"
  , "235, 231"
  , "133, 201"
  , "262, 355"
  , "72, 71"
  , "73, 145"
  , "310, 298"
  , "138, 244"
  , "322, 334"
  , "278, 148"
  , "126, 135"
  , "340, 133"
  , "311, 118"
  , "193, 173"
  , "319, 99"
  , "50, 309"
  , "160, 356"
  , "155, 195"
  , "61, 319"
  , "80, 259"
  , "106, 318"
  , "49, 169"
  , "134, 61"
  , "74, 204"
  , "337, 174"
  , "108, 287"
  ]
