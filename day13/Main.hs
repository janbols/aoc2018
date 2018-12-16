module Main where

import           Data.List
import           Data.Matrix
import           Data.Maybe
import           Data.Ord
import           Debug.Trace
import           Utils

main :: IO ()
main = do
  ptNr <- partSelection
  input <- readFile "./day13/input.txt"
  if ptNr == 1
    then print $ pt1 $ lines input
    else print $ pt2 input

pt1 :: [String] -> ([Cart], Int)
pt1 s = (firstCrash, crashTick)
  where
    paddedLines = linesPadRight '.' s
    tracks = toTracks paddedLines
    carts = toCarts paddedLines
    cartIters = iterCarts tracks carts
    firstCrash = fromJust $ find hasCrash cartIters
    crashTick = fromJust $ findIndex hasCrash cartIters

pt2 :: String -> Int
pt2 s = undefined

type P = (Int, Int)

type Tick = Int

newtype Track =
  Track Char
  deriving (Show)

data Turn
  = L
  | S
  | R
  deriving (Show)

data Cart = Cart
  { p    :: P
  , dir  :: Char
  , turn :: [Turn]
  }

instance Show Cart where
  show (Cart p dir (t:ts)) = "Cart" ++ show p ++ show dir ++ show t

instance Eq Cart where
  (Cart p1 _ _) == (Cart p2 _ _) = p1 == p2

instance Ord Cart where
  compare = comparing p

testFile = readFile "./day13/test.txt"

testLines = ["/->-\\", "|   |  /----\\", "| /-+--+-\\  |", "| | |  | v  |", "\\-+-/  \\-+--/", "  \\------/"]

maxCols ls = maximum $ length <$> ls

padRight :: Int -> a -> [a] -> [a]
padRight total c cs
  | length cs >= total = cs
padRight total c cs = cs ++ replicate (total - length cs) c

linesPadRight :: a -> [[a]] -> [[a]]
linesPadRight c css = padRight maxWidth c <$> css
  where
    maxWidth = maxCols css

parseTrack :: Char -> Track
parseTrack c =
  case c of
    '^' -> Track '|'
    'v' -> Track '|'
    '<' -> Track '-'
    '>' -> Track '-'
    c   -> Track c

parseCart :: P -> Char -> Maybe Cart
parseCart p c =
  case c of
    '^' -> Just (Cart p '^' $ cycle [L, S, R])
    'v' -> Just (Cart p 'v' $ cycle [L, S, R])
    '<' -> Just (Cart p '<' $ cycle [L, S, R])
    '>' -> Just (Cart p '>' $ cycle [L, S, R])
    c   -> Nothing

toTracks s = parseTrack <$> fromLists (linesPadRight ' ' s) :: Matrix Track

toCarts s = sort $ fmap fromJust $ filter isJust $ toList $ mapPos parseCart $ fromLists (linesPadRight ' ' s) :: [Cart]

moveCart :: Matrix Track -> Cart -> Cart
moveCart tracks c@(Cart (x, y) dir turn) =
  case dir of
    '^' -> moveUp tracks c
    '>' -> moveRight tracks c
    'v' -> moveDown tracks c
    '<' -> moveLeft tracks c
    _   -> c

moveUp tracks c@(Cart (x, y) dir turn) =
  case targetTrack of
    Track '|'  -> Cart targetPoint '^' turn
    Track '\\' -> Cart targetPoint '<' turn
    Track '/'  -> Cart targetPoint '>' turn
    Track '+'  -> Cart targetPoint (findTurn $ head turn) $ tail turn
    _ -> error("Cannot match " ++ show c ++ " against " ++ show targetTrack)
  where
    targetPoint = (x - 1, y)
    targetTrack = tracks ! targetPoint
    findTurn L = '<'
    findTurn S = '^'
    findTurn R = '>'

moveDown tracks c@(Cart (x, y) dir turn) =
  case targetTrack of
    Track '|'  -> Cart targetPoint 'v' turn
    Track '\\' -> Cart targetPoint '>' turn
    Track '/'  -> Cart targetPoint '<' turn
    Track '+'  -> Cart targetPoint (findTurn $ head turn) $ tail turn
    _ -> error("Cannot match " ++ show c ++ " against " ++ show targetTrack)
  where
    targetPoint = (x + 1, y)
    targetTrack = tracks ! targetPoint
    findTurn L = '>'
    findTurn S = 'v'
    findTurn R = '<'

moveLeft tracks c@(Cart (x, y) dir turn) =
  case targetTrack of
    Track '-'  -> Cart targetPoint '<' turn
    Track '\\' -> Cart targetPoint '^' turn
    Track '/'  -> Cart targetPoint 'v' turn
    Track '+'  -> Cart targetPoint (findTurn $ head turn) $ tail turn
    _ -> error("Cannot match " ++ show c ++ " against " ++ show targetTrack)
  where
    targetPoint = (x, y-1)
    targetTrack = tracks ! targetPoint
    findTurn L = 'v'
    findTurn S = '<'
    findTurn R = '^'

moveRight tracks c@(Cart (x, y) dir turn) =
  case targetTrack of
    Track '-'  -> Cart targetPoint '>' turn
    Track '\\' -> Cart targetPoint 'v' turn
    Track '/'  -> Cart targetPoint '^' turn
    Track '+'  -> Cart targetPoint (findTurn $ head turn) $ tail turn
    _ -> error("Cannot match " ++ show c ++ " against " ++ show targetTrack)
  where
    targetPoint = (x, y+1)
    targetTrack = tracks ! targetPoint
    findTurn L = '^'
    findTurn S = '>'
    findTurn R = 'v'

moveOne :: Matrix Track -> ([Cart],[Cart]) -> ([Cart], [Cart])
moveOne tracks ([], dones) = ([], dones)
moveOne tracks ((td:tds), dones) = if any (isCrash done) tds then (tds, done:dones) else moveOne tracks (tds, (done:dones))
  where
    done = moveCart tracks td

tickOne:: Matrix Track -> [Cart] -> [Cart]
tickOne tracks carts = todos ++ dones
  where
    sorted = sort carts
    (todos, dones) = moveOne tracks (sorted, [])



hasCrash:: [Cart] -> Bool
hasCrash carts = or $ zipWith isCrash carts (tail carts)

isCrash:: Cart -> Cart -> Bool
isCrash (Cart p1 _ _) (Cart p2 _ _) = p1 == p2

iterCarts:: Matrix Track -> [Cart] -> [[Cart] ]
iterCarts tracks = iterate (tickOne tracks)