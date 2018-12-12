{-# LANGUAGE TupleSections #-}

module Main where

import           Data.List
import qualified Data.Map.Strict as Map
import           Debug.Trace
import           Utils
import           Data.Monoid

main :: IO ()
main = do
  ptNr <- partSelection
  input <- requestInput
  if ptNr == 1
    then print $ pt1 $ read input
    else print $ pt2 $ read input

pt1 :: SNo -> P
pt1 sno = extract_1 $ maximumBy windowComparator $ powergrid sno 3 <$> superGrid 3


pt2 :: SNo -> (P,Int, Int)
pt2 sno = maximumBy windowComparator allWindows
  where
    lookup = (Map.!) $ gridMap sno
--    lookup = power sno
    unfolder :: Window -> Maybe (Window, Window)
    unfolder ((x, y), _, sz) | max x y + sz > 300 = Nothing
    unfolder w@(_, _, sz) = Just (result, result)
      where
        result = increaseWindow lookup w (sz + 1)
    initialSuperGrid = powergrid sno 1 <$> superGrid 1
    allWindows = concatMap (unfoldr unfolder) initialSuperGrid


windowComparator:: Window -> Window -> Ordering
windowComparator (_, pwr1, _) (_, pwr2, _) = compare pwr1 pwr2


type SNo = Int

type P = (Int, Int)
type Power = Int
type Size = Int
type Window = (P, Power, Size)

grid = [(x, y) | x <- [1 .. 300], y <- [1 .. 300]] :: [P]

superGrid size =
  [[(x + diffx,  y + diffy) | diffx <- [0 .. size - 1], diffy <- [0 .. size - 1]] | x <- [1 .. 300-size-1], y <- [1 .. 300-size-1]] :: [[P]]

superGridWithSize size =
  (, size) <$> trace ("Creating supergrid of size " ++ show size) (superGrid size) :: [([P], Size)]

diffGrid:: P -> Size -> Size -> [P]
diffGrid (300,300) _ _ = []
diffGrid (x,y) old new =
  let result = [(x+xdiff, y+ydiff) | xdiff <- [old .. new-1], ydiff <- [0 .. old - 1]]
          ++ [(x+xdiff, y+ydiff) | xdiff <- [0 .. old-1], ydiff <- [old .. new - 1]]
          ++ [(x + new -1, y + new -1)]
  in filter (\(x,y) -> x<=300 && y <= 300) result


powergrid :: SNo -> Size-> [P] -> Window
powergrid sno sz grid =
  let f = power sno
   in (head grid, sum $ f <$> grid, sz)

increaseWindow::(P -> Power) -> Window -> Size -> Window
increaseWindow lookup (p, pwr, sz) newSize =
  if newSize <= sz
    then error "new size must be bigger than current size"
    else (p, pwr + diffPwr, newSize)
      where diffPwr = sum $ lookup <$> diffGrid p sz newSize

power :: SNo -> P -> Power
power sno (x, y) =
  let rackId = x + 10
      start = ((rackId * y) + sno) * rackId
      hs =
        if start < 100
          then 0
          else read [reverse (show start) !! 2]
   in hs - 5

gridMap sno = trace "Building gridMap" $ Map.fromList $ grid `zip` (power sno <$> grid) :: Map.Map P Power

