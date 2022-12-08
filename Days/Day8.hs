import Aoc

type Height = Char
type Tree = (Coord, Height)
type Forrest = [[Tree]]
type Coord = (Int, Int)

data Direction = North | East | South | West deriving (Show, Eq)

directions = [North, East, South, West]

mapCoords :: [String] -> Forrest
mapCoords f = [ [ ((x, y), t) | (t, x) <- zip fl [0..] ] | (fl, y) <- zip f [0..]]

visible :: [Tree] -> [Tree]
visible = snd . foldl (\(h, ls) t@(_,l) -> if l>h then (l, t:ls) else (h, ls)) (minBound, [])

height :: Coord -> Forrest -> Height
height (x, y) forrest = snd $ (forrest !! y) !! x

visibleFrom :: Direction -> Forrest -> [Tree]
visibleFrom West = concat . map visible
visibleFrom North = concat . map visible . transpose
visibleFrom East = concat . map (visible . reverse)
visibleFrom South = concat . map (visible . reverse) . transpose

visibleFromTree :: Coord -> Direction -> Forrest -> [Tree]
visibleFromTree c@(x,y) West forrest = takeUntil ((>= height c forrest) . snd) . reverse . take x $ forrest !! y
visibleFromTree c@(x,y) East forrest = takeUntil ((>= height c forrest) . snd) . drop (x+1) $ forrest !! y
visibleFromTree (x,y) North forrest = visibleFromTree (y,x) West $ transpose forrest
visibleFromTree (x,y) South forrest = visibleFromTree (y,x) East $ transpose forrest

scenicScore :: Coord -> Forrest -> Int
scenicScore c forrest = product $ map (\d -> length $ visibleFromTree c d forrest) directions

part1 :: String -> Int
part1 = sum . map length . nubBy (\x y -> fst x == fst y) . concat . (\x -> map (flip visibleFrom $ x) directions) . mapCoords . lines

part2 :: String -> Int
part2 input = maximum [ scenicScore (x,y) forrest | x <- [0..width], y <- [0..height]]
  where
    forrest = mapCoords $ lines input
    height = length forrest - 1
    width = (length $ head forrest) - 1

main :: IO()
main = do
  args <- getArgs
  contents <- readFile $ head args
  
  putStrLn $ "Solution Day 8 Part 1: " ++ show (part1 contents)
  putStrLn $ "Solution Day 8 Part 2: " ++ show (part2 contents)

