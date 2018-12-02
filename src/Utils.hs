module Utils(partSelection, requestInput, requestMultilineInput) where


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
