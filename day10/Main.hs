module Main where

import           Control.Applicative
import           Control.Arrow
import           Data.Char
import           Data.List
import           Data.Tuple
import           Utils
import           Debug.Trace

main :: IO ()
main = do
  ptNr <- partSelection
  input <- readFile "./day10/input.txt"
  if ptNr == 1
    then writeFile "./day10/output.txt" $ pt1 input
    else print $ pt2 input

pt1 :: String -> String
pt1 s = showStars $ fst $ iterateTillSign (map parseStar $ lines s)

pt2 :: String -> Int
pt2 s = snd $ iterateTillSign (map parseStar $ lines s)


type Star = ((Int, Int),(Int, Int))

parseStar:: String -> Star
parseStar s =
  let (fst, snd) = second (drop 12) $ first (drop 10) $ splitAt (fromJust $ elemIndex '>' s) $ init s
      (p1, p2) = second tail $ splitAt (fromJust $ elemIndex ',' fst) fst
      (v1, v2) = second tail $ splitAt (fromJust $ elemIndex ',' snd) snd
   in ((read $ trim p1, read $ trim p2), (read $ trim v1, read $ trim v2))

iterateTillSign:: [Star] -> ([Star], Int)
iterateTillSign stars =
  let starsProgression = zip (iterate iterateStars stars) [0..]
   in (head $ dropWhile (not . hasSigns . fst) starsProgression)


iterateStars:: [Star] -> [Star]
iterateStars = map nextPosition

nextPosition:: Star -> Star
nextPosition ((x,y), (vx,vy)) = ((x+vx, y+vy), (vx, vy))

hasSigns:: [Star] -> Bool
hasSigns stars =
  let sortedByCol = sort $ fmap fst stars
      groupedByCol = groupBy (\(x1, _) (x2, _) -> x1 == x2) sortedByCol
      rowGroupedByCol = map (map snd) groupedByCol
      diffPerCol = map (\sameCol -> zipWith (-) sameCol (tail sameCol)) rowGroupedByCol
      differsBy1 = fmap (filter (==(-1))) diffPerCol
   in maximum (map length differsBy1 ) >= 7

showStars :: [Star] ->String
showStars  stars =
  let pos = map fst stars
      left = minimum $ map fst pos
      right = maximum $ map fst pos
      top = minimum $ map snd pos
      bottom = maximum $ map snd pos
      isStar = flip elem pos
      matrix = [ [ if isStar (x, y) then '*' else '.' | x <- [left .. right] ] | y <- [top .. bottom] ]
   in unlines matrix


testData =
  [ "position=< 9,  1> velocity=< 0,  2>"
  , "position=< 7,  0> velocity=<-1,  0>"
  , "position=< 3, -2> velocity=<-1,  1>"
  , "position=< 6, 10> velocity=<-2, -1>"
  , "position=< 2, -4> velocity=< 2,  2>"
  , "position=<-6, 10> velocity=< 2, -2>"
  , "position=< 1,  8> velocity=< 1, -1>"
  , "position=< 1,  7> velocity=< 1,  0>"
  , "position=<-3, 11> velocity=< 1, -2>"
  , "position=< 7,  6> velocity=<-1, -1>"
  , "position=<-2,  3> velocity=< 1,  0>"
  , "position=<-4,  3> velocity=< 2,  0>"
  , "position=<10, -3> velocity=<-1,  1>"
  , "position=< 5, 11> velocity=< 1, -2>"
  , "position=< 4,  7> velocity=< 0, -1>"
  , "position=< 8, -2> velocity=< 0,  1>"
  , "position=<15,  0> velocity=<-2,  0>"
  , "position=< 1,  6> velocity=< 1,  0>"
  , "position=< 8,  9> velocity=< 0, -1>"
  , "position=< 3,  3> velocity=<-1,  1>"
  , "position=< 0,  5> velocity=< 0, -1>"
  , "position=<-2,  2> velocity=< 2,  0>"
  , "position=< 5, -2> velocity=< 1,  2>"
  , "position=< 1,  4> velocity=< 2,  1>"
  , "position=<-2,  7> velocity=< 2, -2>"
  , "position=< 3,  6> velocity=<-1, -1>"
  , "position=< 5,  0> velocity=< 1,  0>"
  , "position=<-6,  0> velocity=< 2,  0>"
  , "position=< 5,  9> velocity=< 1, -2>"
  , "position=<14,  7> velocity=<-2,  0>"
  , "position=<-3,  6> velocity=< 2, -1>"
  ]
