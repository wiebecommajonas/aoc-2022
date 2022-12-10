import Aoc
import qualified Data.Map as M

data Inst = Noop | Addx Int deriving (Show, Eq)
type Cycle = Int
type Cpu = (Cycle, Int)

inst :: [String] -> (Inst, [String])
inst ("noop":xs) = (Noop, xs)
inst ("addx":x:xs) = (Addx $ read x, xs)
inst (x:_) = error $ "unknown symbol: " ++ x

parse :: String -> [Inst]
parse = repeatedly inst . words

run1 :: Cpu -> Inst -> Cpu
run1 (cycle, regx) i = case i of
                        Noop -> (cycle+1,regx)
                        Addx n -> (cycle+2, regx+n)

run :: Cpu -> [Inst] -> [Cpu]
run cpu = foldl (\ cs@(c:_) i -> run1 c i : cs) [cpu]

addSignals :: [Cycle] -> [Cpu] -> Int
addSignals [] _ = 0
addSignals _ [] = error "should not occur"
addSignals (n:ns) [c] =   if fst c == n then n*snd c else 0
addSignals (n:ns) (c:d:cs) | fst c == n || (fst c <= n && fst d > n) = n * snd c + addSignals ns (d:cs)
                           | otherwise = addSignals (n:ns) (d:cs)

crtPos :: Cycle -> Int
crtPos cycle = (cycle - 1) `mod` 40

crtChar :: Cpu -> Cycle -> Char
crtChar cpu cycle = if crtPos cycle `elem` [(n-1) .. (n+1)] then 'X' else ' '
  where n = snd cpu

crtLine :: [Cpu] -> [Cycle] -> String
crtLine _ [] = []
crtLine [c] (i:is) = [crtChar c i]
crtLine (c:d:cs) (i:is) | fst d <= i = crtLine (d:cs) (i:is)
                        | otherwise = crtChar c i : crtLine (c:d:cs) is

part1 :: String -> Int
part1 = addSignals [20,60,100,140,180,220] . reverse . run (1,1) . parse

part2 :: String -> String
part2 = unlines . (\x -> map (crtLine x) [ [i .. (i+39)] | i <- [1,41..201] ]) . reverse . run (1,1) . parse

main :: IO()
main = do
  args <- getArgs
  contents <- readFile $ head args
  
  putStrLn $ "Solution Day 10 Part 1: " ++ show (part1 contents)
  putStrLn $ "Solution Day 10 Part 2: \n" ++ (part2 contents)

