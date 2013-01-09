-- Compute a board given Bananagrams pieces.
-- This does not get all valid boards, and
-- not all computed boards will be valid.
--
-- But it should still help.
--
-- There are only 145 letters, so we can make the board twice that big.

import qualified Data.Array as Array
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Tuple as Tuple

-- Board structure
type Board = Array.Array Int (Array.Array Int Char)

boardSide = 292

newBoard :: Board
newBoard = Array.listArray (1, boardSide) [newBoardRow| i <- [1..boardSide]]
  where
    newBoardRow = Array.listArray (1, boardSide) [' '| i <- [1..boardSide]]

-- What does a location have as the word?
contents :: Board -> (Int, Int) -> Int -> String -> String
contents board (startX, startY) wordLength direction
  | direction == "horizontal" = map (\x -> board Array.! x Array.! startX) xRange
  | direction == "vertical" = map (\y -> board Array.! y Array.! startY) yRange
  where
    xRange = [startX..(startX + (wordLength))]
    yRange = [startY..(startY + (wordLength))]

-- Does a word fit at a location?
fits :: String -> String -> Bool
fits boardStrip newWord = all letterMatches $ zip boardStrip newWord
  where
    letterMatches pair = fst pair == ' ' || fst pair == snd pair || ' ' == snd pair 

-- nextWord :: Board -> Board
-- nextWord oldBoard =

placeWord :: Board -> Board
placeWord board = 
  startPoints = [(x,y) | x <- [xMin..xMax], y <- [yMin..yMax]]

buildDict :: [String] -> Map.Map String (Set.Set String)
buildDict words = Map.fromListWith Set.union sortedWords
  where
    sortedWords = map (\ word -> (List.sort word, Set.fromList [word])) words 

main = do
  -- putStrLn $ show $ nextWord' "elephant" "root"
  putStrLn $ show $ fits " H  T" " HI "
  putStrLn $ show $ contents newBoard (82, 132) 4 "vertical"

testFile = do
  f  <- readFile "dict"
  let dict = buildDict $ take 5 $ lines f
  putStrLn $ show $ dict

testDict = do
  putStrLn $ show $ buildDict ["chalk", "reed", "deer"]
  putStrLn $ show $ Map.lookup "achkl" $ buildDict ["chalk", "reed", "deer"]
  putStrLn $ show $ Map.lookup "aal" $ buildDict ["chalk", "reed", "deer"]
