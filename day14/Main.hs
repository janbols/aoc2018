module Main where

import           Data.Foldable
import           Data.List as List
import           Data.Sequence as Seq (Seq, fromList, index, (><), reverse, take)
import           Debug.Trace
import           Utils

main :: IO ()
main = do
  ptNr <- partSelection
  input <- requestInput
  if ptNr == 1
    then print $ pt1 $ read input
    else print $ pt2 $ read input

pt1 :: Int -> String
pt1 cnt = concatMap show $ List.take 10 $ drop cnt $ toList $ fst $ last $ updateTillLength (cnt + 11) s0

pt2 :: Int -> (String,Int)
pt2 cnt = (result, length result)
  where
    result = concatMap show $ toList $ fst $ head $ updateTillInt cnt s0

type Recipes = Seq Int

type CurrentIxs = [Int]

type State = (Recipes, CurrentIxs)

s0 = (fromList [3, 7], [0, 1]) :: State
s0r = (fromList [7, 3], [1,0]) :: State

updateRecipes :: State -> Recipes
updateRecipes (rs, ixs) = rs >< extra
  where
    total = sum $ (\ix -> rs `index` ix) <$> ixs
    extra = fromList (read . (: []) <$> show total)

updateRecipesRev :: State -> Recipes
updateRecipesRev (rs, ixs) = rs >< extra
  where
    total = sum $ (\ix -> rs `index` ix) <$> ixs
    extra = fromList (read . (: []) <$> show total)

updateIxs :: State -> CurrentIxs
updateIxs (rs, ixs) = fmap (updateIx rs) ixs

updateIx :: Recipes -> Int -> Int
updateIx rs ix = (ix + updatedIx) `mod` length rs
  where
    startIx = rs `index` ix
    updatedIx = 1 + startIx

update :: State -> State
update (rs, ixs) = (updatedRecipes, updatedIxs)
  where
    updatedRecipes = updateRecipes (rs, ixs)
    updatedIxs = updateIxs (updatedRecipes, ixs)

updateTillLength :: Int -> State -> [State]
updateTillLength cnt (rs, ixs) = takeWhile (\(rs, _) -> length rs <= cnt) updates
  where
    updates = iterate' update (rs, ixs)

updateTillInt :: Int -> State -> [State]
updateTillInt cnt (rs, ixs) = dropWhile (not . containsCnt) updates
  where
    updates = iterate' update (rs, ixs)
    reverseCnt = List.reverse $ show cnt :: String
    cntLength = length reverseCnt
    containsCnt (recipe, _) = reverseCnt `isInfixOf` target
      where
        target = concatMap show (toList $ Seq.take (cntLength + 1) $ Seq.reverse recipe) :: String --one iteration can only add at most 2 elements so take one more than the cnt
