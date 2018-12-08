{-# LANGUAGE TupleSections #-}

module Main where

import           Data.List
import           Data.Maybe
import           Data.Char
import           Utils
import           Debug.Trace
--import           Control.Applicative

main :: IO ()
main = do
  ptNr <- partSelection
  input <- requestMultilineInput
  if ptNr == 1
    then print $ pt1 $ lines input
    else print $ pt2 $ lines input

pt1 :: [String] -> [Step]
pt1 inputData =
  let instructions = map parseInstr inputData
      allSteps = sort $ nub $ map fst instructions ++ map snd instructions
   in resolveAllSteps allSteps instructions

pt2 :: [String] -> Int
pt2 inputData =
    let is = map parseInstr inputData
        steps = sort $ nub $ map fst is ++ map snd is
  in countAndResolveAllSteps steps is (replicate 5 idleWorker) 0


type Step = Char

type Instr = (Step, Step)

newtype Worker = Worker{unWorker :: (Char, Int)} deriving (Show, Eq)

instance Ord Worker where
  Worker(c1, d1)  `compare` Worker(c2, d2)  =
    case d1 `compare` d2 of
      EQ -> c1 `compare` c2
      GT -> GT
      LT -> LT

idleWorker = Worker('_',0)


empty:: [a] -> Bool
empty [] = True
empty _ = False

workingOn::Step -> [Worker] -> Bool
workingOn step = any (\(Worker(a,d)) -> a == step && d > 0)

freeWorkers:: [Worker] -> [Worker]
freeWorkers = filter ((==0) . snd . unWorker)

wait:: Int -> [Worker] -> [Worker]
wait waitTime = map (\(Worker (c,d)) -> Worker(c, max 0 (d-waitTime)))

addWork:: Step -> [Worker] -> [Worker]
addWork step workers =
  case elemIndex 0 (map (snd . unWorker) workers) of
    Nothing -> workers
    Just ix -> let (f,s) = splitAt ix workers
                   newWorkerAtIx = Worker(step, beginDuration step)
               in f ++ (newWorkerAtIx : tail s)




parseInstr :: String -> Instr
parseInstr s =
  let ws = words s
   in (head (ws !! 1), head (ws !! 7))

resolveAllSteps :: [Step] -> [Instr] -> [Step]
resolveAllSteps steps is =
  case resolveNextSteps steps is of
    [] -> []
    candidates ->
      let nextStep = minimum candidates
      in nextStep : resolveAllSteps (delete nextStep steps) (removeFromInstr nextStep is)


countAndResolveAllSteps :: [Step] -> [Instr] -> [Worker] -> Int -> Int
countAndResolveAllSteps steps is workers time
  | allProcessed = time
  | not (empty availableCandidates) && not (empty availableWorkers) = countAndResolveAllSteps remainingSteps remainingInstructions nextWorkers time
  | otherwise = countAndResolveAllSteps steps is (wait 1 workers) (time + 1)
  where
    allProcessed = null steps && length (freeWorkers workers) == length workers
    availableWorkers = freeWorkers workers
    processedSteps = map (fst . unWorker) availableWorkers
    processingSteps = map (fst . unWorker) workers \\ processedSteps
    remainingInstructions = foldl (\instr f -> f instr) is (map removeFromInstr processedSteps)
    availableCandidates = resolveNextSteps steps remainingInstructions \\ processingSteps
    nextStep = head availableCandidates
    remainingSteps = filter (/= nextStep) steps
    nextWorkers = addWork nextStep $ removeProcessedSteps workers


removeProcessedSteps:: [Worker] -> [Worker]
removeProcessedSteps  = map replaceDoneWithIdle
  where replaceDoneWithIdle:: Worker -> Worker
        replaceDoneWithIdle (Worker(_,0)) = idleWorker
        replaceDoneWithIdle w = w

beginDuration:: Step -> Int
beginDuration s = ord s - ord 'A' + 1 + 60

resolveNextSteps :: [Step] -> [Instr] -> [Step]
resolveNextSteps steps is = resolveNextStepAcc steps is []

resolveNextStepAcc :: [Step] -> [Instr] -> [Step] -> [Step]
resolveNextStepAcc (t:ts) is acc =
  if t `elem` acc
    then resolveNextStepAcc ts is acc
    else let candidateSteps = resolveNonBlockingSteps t is []
          in resolveNextStepAcc ts is (candidateSteps ++ acc)
resolveNextStepAcc [] is acc = nub acc

removeFromInstr :: Step -> [Instr] -> [Instr]
removeFromInstr s = filter ((/= s) . fst)

resolveNonBlockingSteps :: Step -> [Instr] -> [Step] -> [Step]
resolveNonBlockingSteps s is acc =
  let bSteps = blockingSteps s is
   in case bSteps of
        []     -> s : acc
        (b:bs) -> resolveNonBlockingSteps b is acc

blockingSteps :: Step -> [Instr] -> [Step]
blockingSteps s is = map fst $ filter ((== s) . snd) is

testInstr = map parseInstr testData

testData =
  [ "Step C must be finished before step A can begin."
  , "Step C must be finished before step F can begin."
  , "Step A must be finished before step B can begin."
  , "Step A must be finished before step D can begin."
  , "Step B must be finished before step E can begin."
  , "Step D must be finished before step E can begin."
  , "Step F must be finished before step E can begin."
  ]

inputData =
  [ "Step Y must be finished before step L can begin."
  , "Step N must be finished before step D can begin."
  , "Step Z must be finished before step A can begin."
  , "Step F must be finished before step L can begin."
  , "Step H must be finished before step G can begin."
  , "Step I must be finished before step S can begin."
  , "Step M must be finished before step U can begin."
  , "Step R must be finished before step J can begin."
  , "Step T must be finished before step D can begin."
  , "Step U must be finished before step D can begin."
  , "Step O must be finished before step X can begin."
  , "Step B must be finished before step D can begin."
  , "Step X must be finished before step V can begin."
  , "Step J must be finished before step V can begin."
  , "Step D must be finished before step A can begin."
  , "Step K must be finished before step P can begin."
  , "Step Q must be finished before step C can begin."
  , "Step S must be finished before step E can begin."
  , "Step A must be finished before step V can begin."
  , "Step G must be finished before step L can begin."
  , "Step C must be finished before step W can begin."
  , "Step P must be finished before step W can begin."
  , "Step V must be finished before step W can begin."
  , "Step E must be finished before step W can begin."
  , "Step W must be finished before step L can begin."
  , "Step P must be finished before step E can begin."
  , "Step T must be finished before step K can begin."
  , "Step A must be finished before step G can begin."
  , "Step G must be finished before step P can begin."
  , "Step N must be finished before step S can begin."
  , "Step R must be finished before step D can begin."
  , "Step M must be finished before step G can begin."
  , "Step Z must be finished before step L can begin."
  , "Step M must be finished before step T can begin."
  , "Step S must be finished before step L can begin."
  , "Step S must be finished before step W can begin."
  , "Step O must be finished before step J can begin."
  , "Step Z must be finished before step D can begin."
  , "Step A must be finished before step C can begin."
  , "Step P must be finished before step V can begin."
  , "Step A must be finished before step P can begin."
  , "Step B must be finished before step C can begin."
  , "Step R must be finished before step S can begin."
  , "Step X must be finished before step S can begin."
  , "Step T must be finished before step P can begin."
  , "Step Y must be finished before step E can begin."
  , "Step G must be finished before step E can begin."
  , "Step Y must be finished before step K can begin."
  , "Step J must be finished before step P can begin."
  , "Step I must be finished before step Q can begin."
  , "Step E must be finished before step L can begin."
  , "Step X must be finished before step J can begin."
  , "Step T must be finished before step X can begin."
  , "Step M must be finished before step O can begin."
  , "Step K must be finished before step A can begin."
  , "Step D must be finished before step W can begin."
  , "Step H must be finished before step C can begin."
  , "Step F must be finished before step R can begin."
  , "Step B must be finished before step Q can begin."
  , "Step M must be finished before step Q can begin."
  , "Step D must be finished before step S can begin."
  , "Step Y must be finished before step I can begin."
  , "Step M must be finished before step K can begin."
  , "Step S must be finished before step G can begin."
  , "Step X must be finished before step L can begin."
  , "Step D must be finished before step V can begin."
  , "Step B must be finished before step X can begin."
  , "Step C must be finished before step L can begin."
  , "Step V must be finished before step L can begin."
  , "Step Z must be finished before step Q can begin."
  , "Step Z must be finished before step H can begin."
  , "Step M must be finished before step S can begin."
  , "Step O must be finished before step C can begin."
  , "Step B must be finished before step A can begin."
  , "Step U must be finished before step V can begin."
  , "Step U must be finished before step A can begin."
  , "Step X must be finished before step G can begin."
  , "Step K must be finished before step C can begin."
  , "Step T must be finished before step S can begin."
  , "Step K must be finished before step G can begin."
  , "Step U must be finished before step B can begin."
  , "Step A must be finished before step E can begin."
  , "Step F must be finished before step V can begin."
  , "Step Q must be finished before step A can begin."
  , "Step F must be finished before step Q can begin."
  , "Step J must be finished before step L can begin."
  , "Step O must be finished before step E can begin."
  , "Step O must be finished before step Q can begin."
  , "Step I must be finished before step K can begin."
  , "Step I must be finished before step P can begin."
  , "Step J must be finished before step D can begin."
  , "Step Q must be finished before step P can begin."
  , "Step S must be finished before step C can begin."
  , "Step U must be finished before step P can begin."
  , "Step S must be finished before step P can begin."
  , "Step O must be finished before step B can begin."
  , "Step Z must be finished before step F can begin."
  , "Step R must be finished before step V can begin."
  , "Step D must be finished before step L can begin."
  , "Step Y must be finished before step T can begin."
  , "Step G must be finished before step C can begin."
  ]
