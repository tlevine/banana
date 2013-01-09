-- Compute a board given Bananagrams pieces.
-- This does not get all valid boards, and
-- not all computed boards will be valid.
--
-- But it should still help.

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
  
type FirstWord = String
type AttachedWord = (String, Char, String)
type Board = (FirstWord, [AttachedWord], [Char])

-- addWord :: Board -> [Char] -> Board
-- addWord oldBoard pieces = newBoard

buildDict :: [String] -> Map.Map String (Set.Set String)
buildDict words = Map.fromListWith Set.union sortedWords
  where
    sortedWords = map (\ word -> (List.sort word, Set.fromList [word])) words 

main = do
  f  <- readFile "dict"
  let dict = buildDict $ take 5 $ lines f
  putStrLn $ show $ dict

testDict = do
  putStrLn $ show $ buildDict ["chalk", "reed", "deer"]
  putStrLn $ show $ Map.lookup "achkl" $ buildDict ["chalk", "reed", "deer"]
  putStrLn $ show $ Map.lookup "aal" $ buildDict ["chalk", "reed", "deer"]
