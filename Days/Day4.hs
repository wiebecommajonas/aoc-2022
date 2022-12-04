import Aoc

type Pair = (Int, Int)

pairs :: String -> (Pair, Pair)
pairs = cast . map ( map (read :: String -> Int) . splitOn "-") . splitOn ","
  where
    cast [[a,b],[c,d]] = ((a,b),(c,d))

contains :: (Pair, Pair) -> Bool
contains (a, b) = a `inP` b || b `inP` a
  where
    inP (a1, a2) (b1, b2) = a1 >= b1 && a2 <= b2

overlaps :: (Pair, Pair) -> Bool
overlaps ((a1, a2), (b1, b2)) = a \\ b /= a || b \\ a /= b
   where
    a = [a1 .. a2]
    b = [b1 .. b2]

part1 :: String -> Int
part1 = length . filter contains . map pairs . lines

part2 :: String -> Int
part2 = length . filter overlaps . map pairs . lines

main :: IO()
main = do
  args <- getArgs
  contents <- readFile $ head args
  
  putStrLn $ "Solution Day 4 Part 1: " ++ show (part1 contents)
  putStrLn $ "Solution Day 4 Part 2: " ++ show (part2 contents)

