-- Compute a board given Bananagrams pieces.
-- This does not get all valid boards, and
-- not all computed boards will be valid.
--
-- But it should still help.

import qualified Data.Map as Map
import qualified Data.Set as Set
  
type FirstWord = String
type AttachedWord = (String, Char, String)
type Board = (FirstWord, [AttachedWord], [Char])

sample_lengths_dict = Map.fromList [ (7, Set.fromList ["accusal", "obelisk"]), (11, Set.fromList ["inscription", "insecticide"]) ]

-- board :: Map.Map -> String -> [String]
-- board dict "AFTERALASTTHIMBLE" = ["AFTER", "ALAST", "THIMBLE"]

-- Can the word connect?
-- Join the words so far and see whether the new word might connect.
connects :: Set.Set String -> String -> Bool
connects words_so_far new_word = True

addWord :: Board -> [Char] -> Board
addWord oldBoard pieces = newBoard

-- Choose word length combinations
fan :: Int -> [[Int]]
fan npieces
  | npieces < 2 = []
  | otherwise = [[npieces]] ++ soFar
    where
      soFar = map (\ list -> list ++ [2]) (fan (npieces - 1 ))

main = do
  -- This should add ("r", 'o', "und") to the board.
  addWord ("elephant", [("", 'p', "en"), ("ba", 'n', "ana"), ("p", 'o', "tato")]) "rdnu"

  putStrLn $ show $ fan 5
  putStrLn $ show $ fan 4
  putStrLn $ show $ fan 3
  putStrLn $ show $ fan 2

  putStrLn "The rest should be empty."
  putStrLn $ show $ fan 1
  putStrLn $ show $ fan 0
  putStrLn $ show $ fan ( - 24)
  putStrLn $ show $ fan ( -2342)
