import Aoc

prio :: Char -> Int
prio c | isUpper c = ord c - ord 'A' + 27
       | True = ord c - ord 'a' + 1

doubleprio :: String -> String -> Int
doubleprio (f:fh) lh | f `elem` lh = prio f
                     | True = doubleprio fh lh

splitHalf :: String -> (String, String)
splitHalf str = (take len str, drop len str)
  where
    len = length str `div` 2

part1 :: String -> Int
part1 = sum . map (uncurry doubleprio . splitHalf) . lines

groupSeq :: [String] -> [[String]]
groupSeq [] = []
groupSeq xs = take 3 xs : groupSeq (drop 3 xs)

badge :: [String] -> Int
badge [(a:as),b,c] | a `elem` b && a `elem` c = prio a
                   | True = badge [as,b,c]

part2 :: String -> Int
part2 = sum . map badge . groupSeq . lines

main :: IO()
main = do
  args <- getArgs
  contents <- readFile $ head args
  
  putStrLn $ "Solution Day 1 Part 1: " ++ show (part1 contents)
  putStrLn $ "Solution Day 1 Part 2: " ++ show (part2 contents)

