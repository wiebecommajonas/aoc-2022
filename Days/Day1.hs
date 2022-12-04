import Aoc

part1 :: String -> Int
part1 = maximum . map (sum . map (read :: String -> Int)) . map words . splitOn "\n\n"

part2 :: String -> Int
part2 str = help 3 cs
  where
    help 0 _  = 0
    help n cs = maximum cs + help (n-1) (cs \\ [maximum cs])
    cs = map (sum . map (read :: String -> Int)) . map words . splitOn "\n\n" $ str

main :: IO()
main = do
  args <- getArgs
  contents <- readFile $ head args
  
  putStrLn $ "Solution Day 1 Part 1: " ++ show (part1 contents)
  putStrLn $ "Solution Day 1 Part 2: " ++ show (part2 contents)

