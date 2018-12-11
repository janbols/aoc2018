module Utils(partSelection, requestInput, requestMultilineInput, extract_1, extract_2, extract_3, partition, partitionAt, trim) where

import Data.Char

partSelection:: IO Int
partSelection = do
    _ <- putStrLn "Select part 1 or 2... [1/2]"
    input <- getLine
    if read input == 1 then selectedPart 1
      else if read input == 2 then selectedPart 2
        else partSelection

selectedPart:: Int -> IO Int
selectedPart pt = do
  _ <- putStrLn $ "Selected part: " ++ show pt
  return pt

requestInput:: IO String
requestInput = do
  _ <- putStrLn "Input..."
  getLine


requestMultilineInput:: IO String
requestMultilineInput = do
    _ <- putStrLn "Input... (finish with Ctrl+D)"
    getContents


extract_1:: (a,b,c) -> a
extract_1 (a,_,_) = a

extract_2:: (a,b,c) -> b
extract_2 (_,b,_) = b

extract_3:: (a,b,c) -> c
extract_3 (_,_,c) = c


partitionAt :: (a->Bool) -> [a] -> [[a]]
partitionAt _ [] = []
partitionAt f (x:xs) = (x : takeWhile (not . f) xs) : partitionAt f (dropWhile (not . f) xs)

partition :: Int -> [a] -> [[a]]
partition _ [] = []
partition n xs = take n xs : partition n (drop n xs)

trim:: String -> String
trim = dropWhile isSpace
