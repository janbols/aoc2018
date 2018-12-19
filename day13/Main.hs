module Main where

import           Data.List
import           Data.Matrix
import           Data.Maybe
import           Data.Ord
import           Data.String
import           Debug.Trace
import           Utils

main :: IO ()
main = do
  ptNr <- partSelection
  input <- readFile "./day13/input.txt"
  if ptNr == 1
    then print $ pt1 $ lines input
    else print $ pt2 $ lines input

pt1 :: [String] -> P
pt1 s = (y-1, x-1)
  where
    paddedLines = linesPadRight '.' s
    tracks = toTracks paddedLines
    carts = toCarts paddedLines
    cartIters = iterCarts tracks carts
    firstCrash = fromJust $ find (isJust . crashedCarts) cartIters
    (x,y) = p $ head $ fromJust $ crashedCarts firstCrash

pt2 :: [String] -> P
pt2 s = (y - 1, x - 1)
  where
    paddedLines = linesPadRight '.' s
    tracks = toTracks paddedLines
    carts = toCarts paddedLines
    cartIters = iterCartsAndRemove tracks carts
    onlyCarLeft = dropWhile (\cs -> length cs > 1) cartIters !! 1
    (x, y) = p $ head onlyCarLeft


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
  { id   :: Int
  , p    :: P
  , dir  :: Char
  , turn :: Turn
  } deriving (Show)

instance Eq Cart where
  (Cart id1 _ _ _) == (Cart id2 _ _ _) = id1 == id2

instance Ord Cart where
  compare = comparing p

nextTurn:: Turn-> Turn
nextTurn L = S
nextTurn S = R
nextTurn R = L

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
parseCart p@(x, y) c =
  case c of
    '^' -> Just (Cart 0 p '^' L)
    'v' -> Just (Cart 0 p 'v' L)
    '<' -> Just (Cart 0 p '<' L)
    '>' -> Just (Cart 0 p '>' L)
    c   -> Nothing

toTracks s = parseTrack <$> fromLists (linesPadRight ' ' s) :: Matrix Track

toCarts s = sort cards :: [Cart]
  where
    anonymousCards = fmap fromJust $ filter isJust $ toList $ mapPos parseCart $ fromLists (linesPadRight ' ' s)
    cards = (\(id, Cart _ p d t) -> Cart id p d t) <$> zip [1 ..] anonymousCards




moveCart :: Matrix Track -> Cart -> Cart
moveCart tracks c@(Cart _ _ dir _) =
  case dir of
    '^' -> moveUp tracks c
    '>' -> moveRight tracks c
    'v' -> moveDown tracks c
    '<' -> moveLeft tracks c
    _   -> c

moveUp tracks c@(Cart id (x, y) dir turn) =
  case targetTrack of
    Track '|' -> Cart id targetPoint '^' turn
    Track '\\' -> Cart id targetPoint '<' turn
    Track '/' -> Cart id targetPoint '>' turn
    Track '+' -> Cart id targetPoint (findDir turn) $ nextTurn turn
    _ -> error ("Cannot match " ++ show c ++ " against " ++ show targetTrack)
  where
    targetPoint = (x - 1, y)
    targetTrack = tracks ! targetPoint
    findDir L = '<'
    findDir S = '^'
    findDir R = '>'

moveDown tracks c@(Cart id (x, y) dir turn) =
  case targetTrack of
    Track '|' -> Cart id targetPoint 'v' turn
    Track '\\' -> Cart id targetPoint '>' turn
    Track '/' -> Cart id targetPoint '<' turn
    Track '+' -> Cart id targetPoint (findDir turn) $ nextTurn turn
    _ -> error ("Cannot match " ++ show c ++ " against " ++ show targetTrack)
  where
    targetPoint = (x + 1, y)
    targetTrack = tracks ! targetPoint
    findDir L = '>'
    findDir S = 'v'
    findDir R = '<'

moveLeft tracks c@(Cart id (x, y) dir turn) =
  case targetTrack of
    Track '-' -> Cart id targetPoint '<' turn
    Track '\\' -> Cart id targetPoint '^' turn
    Track '/' -> Cart id targetPoint 'v' turn
    Track '+' -> Cart id targetPoint (findDir turn) $ nextTurn turn
    _ -> error ("Cannot match " ++ show c ++ " against " ++ show targetTrack)
  where
    targetPoint = (x, y - 1)
    targetTrack = tracks ! targetPoint
    findDir L = 'v'
    findDir S = '<'
    findDir R = '^'

moveRight tracks c@(Cart id (x, y) dir turn) =
  case targetTrack of
    Track '-' -> Cart id targetPoint '>' turn
    Track '\\' -> Cart id targetPoint 'v' turn
    Track '/' -> Cart id targetPoint '^' turn
    Track '+' -> Cart id targetPoint (findDir turn) $ nextTurn turn
    _ -> error ("Cannot match " ++ show c ++ " against " ++ show targetTrack)
  where
    targetPoint = (x, y + 1)
    targetTrack = tracks ! targetPoint
    findDir L = '^'
    findDir S = '>'
    findDir R = 'v'

tickOne :: Matrix Track -> [Cart] -> [Cart]
tickOne tracks carts = Debug.Trace.trace (showIds (sort $ todos ++ dones)) (todos ++ dones)
  where
    go :: ([Cart], [Cart]) -> ([Cart], [Cart])
    go ([], dones) = ([], dones)
    go (td:tds, dones) =
      if any (isCrash moved) tds || any (isCrash moved) dones
        then Debug.Trace.trace ("crash!!" ++ show moved) (tds, moved : dones)
        else go (tds, moved : dones)
      where
        moved = moveCart tracks td
    (todos, dones) = go (sort carts, [])

showIds carts = unlines $ (\ (Cart id (x,y) dir t) -> show (y-1) ++ "," ++ show (x-1) ++ " " ++ show dir) <$> carts


isCrash :: Cart -> Cart -> Bool
isCrash (Cart _ p1 _ _) (Cart _ p2 _ _) = p1 == p2

crashedCarts :: [Cart] -> Maybe [Cart]
crashedCarts carts = find (\ cs -> length cs > 1) grouped
  where
    grouped = groupBy (\c1 c2 -> p c1 == p c2) carts

iterCarts :: Matrix Track -> [Cart] -> [[Cart]]
iterCarts tracks = iterate (sort . tickOne tracks)

removeCrashed:: [Cart] -> [Cart]
removeCrashed carts = concat (filter (\cs -> length cs == 1) grouped)
  where
    grouped = groupBy (\c1 c2 -> p c1 == p c2) carts


iterCartsAndRemove :: Matrix Track -> [Cart] -> [[Cart]]
iterCartsAndRemove tracks = iterate (removeCrashed . sort . tickOne tracks)

