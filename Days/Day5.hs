import Aoc

type Crate = Char
type Inst = (Int, Int, Int)

crate :: String -> [Crate]
crate ('[':x:_) = [x]
crate _ = []

collectCrates :: String -> [[Crate]]
collectCrates input = map crate . chunksOf 4 $ input

collectAllCrates :: [String] -> [[Crate]]
collectAllCrates = foldl (fmap (map $ uncurry (++)) . zip) (take 9 $ repeat []) . map collectCrates

parse :: String -> Inst
parse = castT3 . map read . odds . words

move9000 :: Inst -> [[Crate]] -> [[Crate]]
move9000 (0,_,_) cs = cs
move9000 (num,from,to) cs = move9000 (num-1, from, to) . replaceAt (from-1) fromNew . replaceAt (to-1) toNew $ cs
  where
    fromEl = head $ cs !! (from - 1)
    fromNew = tail $ cs !! (from - 1)
    toNew = fromEl : cs !! (to - 1)

move9001 :: Inst -> [[Crate]] -> [[Crate]]
move9001 (num,from,to) cs = replaceAt (from-1) fromNew . replaceAt (to-1) toNew $ cs
  where
    fromEl = take num $ cs !! (from - 1)
    fromNew = drop num $ cs !! (from - 1)
    toNew = fromEl ++ cs !! (to - 1)

moveWith :: (Inst -> [[Crate]] -> [[Crate]]) -> [Inst] -> [[Crate]] -> [[Crate]]
moveWith _ [] cs = cs
moveWith m (i:is) cs = moveWith m is $ m i cs

solveWith :: (Inst -> [[Crate]] -> [[Crate]]) -> String -> String
solveWith m input = map head . moveWith m is $ cs
  where
    [crates, instructions] = map lines . splitOn "\n\n" $ input
    cs = collectAllCrates crates
    is = map parse $ instructions

part1 :: String -> String
part1 = solveWith move9000

part2 :: String -> String
part2 = solveWith move9001

main :: IO()
main = do
  args <- getArgs
  contents <- readFile $ head args
  
  putStrLn $ "Solution Day 5 Part 1: " ++ show (part1 contents)
  putStrLn $ "Solution Day 5 Part 2: " ++ show (part2 contents)
