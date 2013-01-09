import qualified Data.Map as Map
  
builddict :: [String] -> Data.Map
builddict [] = 
builddict 

fromList [(5,'a'), (3,'b')]

sample_lengths_dict = Map.fromList [
    (7, Set.fromList ["accusal", "obelisk"]),
    (11, Set.fromList ["inscription", "insecticide"])
]

board :: Map -> String -> [String]
board dict "AFTERALASTTHIMBLE" = ["AFTER", "ALAST", "THIMBLE"]

-- Can the word connect?
-- Join the words so far and see whether the new word might connect.
connects :: Set -> String -> Bool
connects words_so_far new_word =

-- Choose word length combinations
fan :: Int -> [[Int]]
fan 1 = [[2]]
fan 2 = [[3], [2,2]]
fan piecesminusone = [piecesminusone + 1] ++ (map getPrevious (reverse [2..piecesminusone]))
  where
    getPrevious i = [i] ++ (fan piecesminusone)
