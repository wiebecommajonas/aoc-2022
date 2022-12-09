import Aoc
import qualified Data.Set as S

type Coord = (Int, Int)
data Dir = North | East | South | West deriving (Eq, Show)
data Inst = Move Dir Int deriving (Eq, Show)
data Path = Path [Coord]

instance Show Path where
  show (Path cs) = unlines $ reverse [ [ if (x,y) `elem` cs then '#' else '.' | x <- [minX .. maxX] ] | y <- [minY .. maxY]]
    where
      minWith f = minimum $ map f cs
      maxWith f = maximum $ map f cs
      (minX, maxX) = (minWith fst, maxWith fst)
      (minY, maxY) = (minWith snd, maxWith snd)

inst :: String -> String -> Inst
inst str num = case str of
                "R" -> Move East n
                "L" -> Move West n
                "U" -> Move North n
                "D" -> Move South n
  where n = read num

parse :: String -> [Inst]
parse = map (uncurry inst . castT2 . words) . lines

diffs :: Coord -> Coord -> [Int]
diffs (x1,y1) (x2,y2) = [x2-x1, y2-y1]

dist :: Coord -> Coord -> Int
dist x y = maximum . map abs $ diffs x y

diag :: Coord -> Coord -> Bool
diag x y = all (/=0) $ diffs x y

chase1 :: Coord -> Coord -> Coord
chase1 h t | dist h t <= 1 = t -- should never be > 2
           | otherwise = (uncurry (***)) (castT2 . map diagFn $ diffs t h) t
  where
    diagFn i | i < 0 = subtract 1
             | i > 0 = (+) 1
             | otherwise = id

chase :: Coord -> [Coord] -> [Coord]
chase _ [] = []
chase h (t:ts) = newT : chase newT ts
  where
    newT = chase1 h t

move :: Coord -> Inst -> Coord
move (x,y) (Move East  _) = (x+1,y  )
move (x,y) (Move West  _) = (x-1,y  )
move (x,y) (Move North _) = (x  ,y+1)
move (x,y) (Move South _) = (x  ,y-1)

traceMove :: Coord -> [Coord] -> Inst -> ((Coord, [Coord]), S.Set Coord)
traceMove head tail (Move _ 0) = ((head, tail), S.singleton (last tail))
traceMove head tail inst@(Move d n) = ((resultHead, resultTail), S.singleton (last tail) `S.union` next)
  where
    newHead = move head inst
    newTail = chase newHead tail
    ((resultHead, resultTail), next) = traceMove newHead newTail (Move d (n-1))

traceMoves :: Coord -> [Coord] -> [Inst] -> S.Set Coord
traceMoves _ t [] = S.singleton $ last t
traceMoves h t (i:is) = visited `S.union` traceMoves newHead newTail is
  where
    ((newHead, newTail), visited) = traceMove h t i

part1 :: String -> S.Set Coord
part1 = traceMoves (0,0) [(0,0)] . parse

part2 :: String -> S.Set Coord
part2 = traceMoves (0,0) (take 9 $ repeat (0,0)) . parse

result :: S.Set Coord -> Int
result = S.size

visualize :: S.Set Coord -> Path
visualize = Path . S.toList

main :: IO()
main = do
  args <- getArgs
  contents <- readFile $ head args
  
  if "--visualize" `elem` args
    then do
      putStrLn $ "Visualization Day 9 Part 1:\n" ++ (show . visualize $ part1 contents)
      putStrLn $ "Visualization Day 9 Part 2:\n" ++ (show . visualize $ part2 contents)
    else do
      putStrLn $ "Solution Day 9 Part 1: " ++ (show . result $ part1 contents)
      putStrLn $ "Solution Day 9 Part 2: " ++ (show . result $ part2 contents)

