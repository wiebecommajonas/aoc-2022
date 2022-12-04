import Aoc

type Pair = (Int, Int)

pairs :: String -> (Pair, Pair)
pairs = cast . map ( map (read :: String -> Int) . splitOn "-") . splitOn ","
  where
    cast [[a,b],[c,d]] = ((a,b),(c,d))

range :: (Int, Int) -> [Int]
range (x, y) = [x .. y]

mapT f (a,b) = (f a, f b)

contains :: (Pair, Pair) -> Bool
contains ps@(a, b) = (\x -> x == range a || x == range b) . uncurry intersect . mapT range $ ps

overlaps :: (Pair, Pair) -> Bool
overlaps = (/= []) . uncurry intersect . mapT range

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

