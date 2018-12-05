{-# LANGUAGE NamedFieldPuns #-}

module Main where

import           Data.Char
import           Data.List
import           Data.List.Split
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
  let ss = lines input
      points = map (claimToPoints . extractClaim) ss
      unioned = concat points
      merged = mergePoints unioned
      filtered = filter (\(PointOcc (pos, ids)) -> length ids > 1) merged
   in length filtered

pt2 :: String -> String
pt2 input =
  let ss = lines input
      claims = map extractClaim ss
      points = map claimToPoints claims
      unioned = concat points
      merged = mergePoints unioned
      overlappingPoints = filter (( > 1) . length  . snd . unPointOcc ) merged
      overlappingIds = nub $ concatMap (snd . unPointOcc) overlappingPoints
      allIds = map (extract_1 . unClaim) claims
  in head (allIds \\ overlappingIds)
--  let ss = lines input
--      claims = map (claimToClaimWithPos . extractClaim) ss
--      ftr = not . flip overlapsAny claims
--      noneOverlapping = filter ftr claims
--  in fst $ unClaimWithPos $ head noneOverlapping


type Point = (Int, Int)

type Dim = (Int, Int)

newtype Claim = Claim
  { unClaim :: (String, Point, Dim)
  } deriving (Show, Eq)

newtype PointOcc = PointOcc
  { unPointOcc  :: (Point, [String])
  } deriving (Show, Eq)

instance Ord PointOcc where
  compare (PointOcc (p1, ids1)) (PointOcc (p2, ids2)) = compare p1 p2


extractClaim :: String -> Claim
extractClaim s =
  let ws = words s
      id = ws !! 0
      left = takeWhile (/= ',') $ ws !! 2
      top = init $ drop (1 + length left) $ ws !! 2
      width = takeWhile (/= 'x') $ ws !! 3
      height = drop (1 + length width) $ ws !! 3
   in Claim (id, (read left, read top), (read width, read height))

claimToPoints :: Claim -> [PointOcc]
claimToPoints (Claim (cid, (left, top), (width, height))) =
  [PointOcc( (x, y), [cid]) | x <- [left .. left + width - 1], y <- [top .. top + height - 1]]

mergePoints :: [PointOcc] -> [PointOcc]
mergePoints ps =
  let grouped = groupBy (\(PointOcc (pos1, ids1)) (PointOcc (pos2, ids2)) -> pos1 == pos2) $ sort ps
      merger points = PointOcc (fst . unPointOcc $ head points, concatMap (snd . unPointOcc) points)
   in map merger grouped
