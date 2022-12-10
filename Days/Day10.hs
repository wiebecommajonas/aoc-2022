import Aoc
import qualified Data.Map as M
import Text.Parsec.String (Parser)
import Text.Parsec.Char (char, digit, space, endOfLine, string)
import Text.Parsec (eof, many, manyTill, parse, (<|>))

data Inst = Noop | Addx Int deriving (Show, Eq)
type Cpu = (Int, Int)

integer :: Parser Int
integer = many digit >>= return . read

signedInteger :: Parser Int
signedInteger = (char '-' >> integer >>= return . negate) <|> integer

noop :: Parser Inst
noop = string "noop" >> return Noop

addx :: Parser Inst
addx = string "addx" >> space >> signedInteger >>= return . Addx

inst :: Parser Inst
inst = (noop <|> addx) >>= (\i -> endOfLine >> return i)

insts :: Parser [Inst]
insts = manyTill inst eof

parseInsts :: String -> [Inst]
parseInsts input = case parse insts "" input of
                Left e -> error $ show e
                Right s -> s

run1 :: Cpu -> Inst -> Cpu
run1 (cycle, regx) i = case i of
                        Noop -> (cycle+1,regx)
                        Addx n -> (cycle+2, regx+n)

run :: Cpu -> [Inst] -> [Cpu]
run cpu = foldl (\ cs@(c:_) i -> run1 c i : cs) [cpu]

addSignals :: [Int] -> [Cpu] -> Int
addSignals [] _ = 0
addSignals _ [] = error "should not occur"
addSignals (n:ns) [c] =   if fst c == n then n*snd c else 0
addSignals (n:ns) (c:d:cs) | fst c == n || (fst c <= n && fst d > n) = n * snd c + addSignals ns (d:cs)
                           | otherwise = addSignals (n:ns) (d:cs)

crtPos :: Int -> Int
crtPos cycle = (cycle - 1) `mod` 40

crtChar :: Cpu -> Int -> Char
crtChar cpu cycle = if crtPos cycle `elem` [(n-1) .. (n+1)] then '#' else '.'
  where n = snd cpu

crtLine :: [Cpu] -> [Int] -> String
crtLine _ [] = []
crtLine [c] (i:is) = [crtChar c i]
crtLine (c:d:cs) (i:is) | fst d <= i = crtLine (d:cs) (i:is)
                        | otherwise = crtChar c i : crtLine (c:d:cs) is

part1 :: String -> Int
part1 = addSignals [20,60,100,140,180,220] . reverse . run (1,1) . parseInsts

part2 :: String -> String
part2 = unlines . (\x -> map (crtLine x) [ [i .. (i+39)] | i <- [1,41..201] ]) . reverse . run (1,1) . parseInsts

main :: IO()
main = do
  args <- getArgs
  contents <- readFile $ head args
  
  putStrLn $ "Solution Day 10 Part 1: " ++ show (part1 contents)
  putStrLn $ "Solution Day 10 Part 2: \n" ++ (part2 contents)

