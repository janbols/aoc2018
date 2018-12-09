{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Prelude hiding (length, zip)
import qualified Data.List as List
import           Utils
import           Debug.Trace
import           Data.Sequence as Seq

main :: IO ()
main = do
  ptNr <- partSelection
  if ptNr == 1
    then do
           _ <- putStrLn "nr of players..."
           nrOfPlayers <- getLine
           _ <- putStrLn "max marble..."
           maxMarble <- getLine
           print $ pt1 (read nrOfPlayers) (read maxMarble)
    else do
           input <- requestInput
           print $ pt2 input

pt1 :: Int -> Int -> Seq Score
pt1 nrOfPlayers maxMarble = go stackOfMarbles beginCircle beginCurrent beginScore
  where stackOfMarbles = List.zip (cycle [0..nrOfPlayers-1]) [1..maxMarble]
        beginCircle = singleton 0
        beginCurrent = (0,0)
        beginScore = fromList $ List.replicate nrOfPlayers 0

        go:: [(Player,Marble)] -> Seq Marble -> Current  -> Seq Score -> Seq Score
        go [] circle current score = score
        go ((p,m):restOfStack) circle (currentIx, currentMarble) score | mod m 23 == 0 =
           go restOfStack circleRemoved newCurrent newScore
            where removePosition = cycleMove circle (\ i -> i-7) currentIx
                  removedMarble = index circle removePosition
                  circleRemoved = deleteAt removePosition circle
                  newCurrent = (removePosition, index circleRemoved removePosition)
                  newScore = adjust (\s -> s + removedMarble + m) p score

        go ((p,m):restOfStack) circle (currIx, currMarble) score=
          go restOfStack circleInserted (insertPosition, m) score
            where insertPosition = cycleMove circle (+2) currIx
                  circleInserted = insertAt insertPosition m circle


pt2 :: String -> Int
pt2 s = undefined

type Player = Int
type Marble = Int
type Current = (Int, Marble)
type Score = Int


cycleMove:: Seq a -> (Int -> Int) -> (Int -> Int)
cycleMove c f = (\ i -> if i == 0 then Seq.length c else i) . (\ i -> mod i $ Seq.length c) . f



