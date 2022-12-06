import Aoc

detectMarker :: Int -> String -> Int
detectMarker n str@(x:xs) | not . anySame . take n $ str = n
                          | otherwise                    = 1 + detectMarker n xs

part1 :: String -> Int
part1 = detectMarker 4

part2 :: String -> Int
part2 = detectMarker 14

main :: IO()
main = do
  args <- getArgs
  contents <- readFile $ head args
  
  putStrLn $ "Solution Day 6 Part 1: " ++ show (part1 contents)
  putStrLn $ "Solution Day 6 Part 2: " ++ show (part2 contents)

