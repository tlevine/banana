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
  

-- Board structure
type Board = Array.Array Integer (Array.Array Integer Char)

boardSide = 292

newBoard :: Board
newBoard = listArray (1, boardSide) [newBoardRow| i <- [1..boardSide]]
  where
    newBoardRow = listArray (1, boardSide) [' '| i <- [1..boardSide]]

firstWord :: String -> Board
firstWord word = newBoard//[((startPointX,startPointY + y), word !! y) | y <- [0..((length word)-1)]]
  where
    startPointX = round $ boardSide / 2
    startPointY = round $ boardSide / 2

-- Does a word fit at a location?
fits :: Board -> String -> (Int, Int) -> String
fits board word (startX, startY) direction =
  | direction == "horizontal" = map (\y -> board ! y ! startY) yRange
  | direction == "vertical" = map (\x -> board ! x ! startx) yRange
  where
    xRange = [startX..(startX + (length word))]
    yRange = [startY..(startY + (length word))]

nextWord :: Board -> Board
nextWord oldBoard =

buildDict :: [String] -> Map.Map String (Set.Set String)
buildDict words = Map.fromListWith Set.union sortedWords
  where
    sortedWords = map (\ word -> (List.sort word, Set.fromList [word])) words 

main = do
  putStrLn $ show $ nextWord' "elephant" "root"

testFile = do
  f  <- readFile "dict"
  let dict = buildDict $ take 5 $ lines f
  putStrLn $ show $ dict

testDict = do
  putStrLn $ show $ buildDict ["chalk", "reed", "deer"]
  putStrLn $ show $ Map.lookup "achkl" $ buildDict ["chalk", "reed", "deer"]
  putStrLn $ show $ Map.lookup "aal" $ buildDict ["chalk", "reed", "deer"]
