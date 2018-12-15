module Main where

import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Debug.Trace
import           Utils

main :: IO ()
main = do
  ptNr <- partSelection
  input <- requestInput
  if ptNr == 1
    then print $ show $ pt1 (read input) initState inputRules
    else print $  show $ pt2 (read input) initState inputRules

pt1 :: Int -> String -> [Rule] -> Int
pt1 nrOfGens state rules = sum $ fst <$> filter (\ (_,c) -> c=='#') zipped
  where
    generations = iterate' (runRules rules) $ pad nrOfGens state
    generationX = generations!!nrOfGens
    zipped = zip [-1*nrOfGens..] generationX

pt2 :: Int ->String -> [Rule] -> Int
pt2 nrOfGens state rules =  sum $ fst <$> filter (\ (_,c) -> c=='#') zipped
  where
    cyclus = findCyclus (runRulesAndPadAndShrink rules) state
    (startOfCyclus, _) = trace ("found cyclus:" ++ show (fst cyclus) ++ "," ++ show (snd cyclus)) cyclus
    generations = iterate' (runRules rules) $ pad startOfCyclus state
    generationX = generations!!startOfCyclus
    offsetDiff = nrOfGens - startOfCyclus
    zipped = zip [-1*startOfCyclus + offsetDiff ..] generationX

findCyclus:: (String-> String) -> String -> (Int,Int)
findCyclus f ist = go ist []
  where
    go s acc =
      if s `elem` acc then (length acc - (fromJust $ s `elemIndex` acc) -1 , length acc)
      else trace ("input length " ++ show (s) ++ " array length " ++ show (length acc)) (go (f s) $ s:acc)

runRulesAndPadAndShrink:: [Rule] -> String -> String
runRulesAndPadAndShrink rules s = stripDots $ (singlePass rules) <$> partitionToRulePatterns padded
  where
    padPattern = "...."
    padded = padPattern ++ (stripDots s) ++ padPattern

stripDots s =  dropWhileEnd (=='.') $ dropWhile (=='.') s


runRules:: [Rule] -> String -> String
runRules rules s = map (singlePass rules) $ partitionToRulePatterns s


singlePass :: [Rule] -> String -> Char
singlePass rules pattern = fromJust $ fromMaybe defaultVal $ find isJust $ (\r -> r pattern) <$> rules
  where
    defaultVal = Just '.'

partitionToRulePatterns :: String -> [Pattern]
partitionToRulePatterns s = l2 `zipper` (l1 `zipper` (s `zipper` (r1 `zipper` ((: []) <$> r2))))
  where
    l1 = "." ++ s
    l2 = "." ++ l1
    r1 = tail s ++ repeat '.'
    r2 = tail r1
    zipper = zipWith (:)

type Pattern = String
type Rule = Pattern -> Maybe Char

parseRule :: String -> Rule
parseRule s pattern =
  if pattern == take 5 s
    then Just (last s)
    else Nothing

initState = ".##.##...#.###..#.#..##..###..##...####.#...#.##....##.#.#...#...###.........##...###.....##.##.##"

inputRules = map parseRule inputRuleDefs

inputRuleDefs =
  [ "##... => ."
  , "#...# => ."
  , ".###. => #"
  , ".##.# => #"
  , "#.... => ."
  , "..##. => #"
  , "##..# => #"
  , ".#... => #"
  , ".#.## => #"
  , "#.### => #"
  , ".#..# => ."
  , "##.#. => #"
  , "..#.. => ."
  , ".##.. => #"
  , "###.# => ."
  , ".#### => ."
  , "##### => ."
  , "#.#.. => #"
  , "...## => #"
  , "...#. => ."
  , "###.. => ."
  , "..... => ."
  , "#.#.# => ."
  , "##.## => #"
  , "#.##. => #"
  , "####. => #"
  , "#..#. => #"
  , ".#.#. => ."
  , "#..## => #"
  , "....# => ."
  , "..#.# => #"
  , ".### => ."
  ]

pad x s = (take x $ repeat '.') ++ s ++ (take x $ repeat '.')


testInitState = "#..#.#..##......###...###"

testInputRules = map parseRule testInputRuleDefs

testInputRuleDefs =
  [ "...## => #"
  , "..#.. => #"
  , ".#... => #"
  , ".#.#. => #"
  , ".#.## => #"
  , ".##.. => #"
  , ".#### => #"
  , "#.#.# => #"
  , "#.### => #"
  , "##.#. => #"
  , "##.## => #"
  , "###.. => #"
  , "###.# => #"
  , "####. => #"
  ]




