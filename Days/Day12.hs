import Aoc
-- import qualified Data.Heap as H
-- import qualified Data.Set as S
-- import qualified Data.Map as M

-- data Direction = North | East | South | West deriving (Show, Eq, Enum, Bounded)
-- type Elev = Char
-- type Coord = (Int, Int)
-- type Path = [(Coord, Elev)]
-- type ElevMap = [Path]

-- directions = [minBound .. maxBound]

-- mapCoords :: [[Elev]] -> ElevMap
-- mapCoords es = [ [ ((x,y),e) | (x,e) <- zip [0..] l ] | (y,l) <- zip [0..] es ]

-- special :: Elev -> ElevMap -> (Coord, Elev)
-- special _ [] = error "only use with S and E"
-- special c (e:es) | isJust found = fromJust found
--                  | otherwise    = special c es
--   where found = find ((==c) . snd) e

-- start, end :: ElevMap -> (Coord, Elev)
-- start = special 'S'
-- end = special 'E'

-- isEnd :: Elev -> Bool
-- isEnd = (==) 'E'

-- real :: Elev -> Elev
-- real c = case c of
--           'S' -> 'a'
--           'E' -> 'z'
--           _ -> c

-- move :: Direction -> Coord -> Coord
-- move North (x,y) = (x  ,y+1)
-- move South (x,y) = (x  ,y-1)
-- move East  (x,y) = (x+1,y  )
-- move West  (x,y) = (x-1,y  )

-- point :: ElevMap -> Coord -> (Coord, Elev)
-- point es (x,y) = (es !! y) !! x

-- nb4 :: Coord -> ElevMap -> [(Coord, Elev)]
-- nb4 c es = map (point es) . filter inBounds $ map ((flip move) c) directions
--   where
--     height = length es
--     width = length $ head es
--     inBounds (x,y) = x >= 0 && y >= 0 && x < width && y < height

-- safe :: Elev -> Elev -> Bool
-- safe a b = rb <= succ ra
--   where (ra, rb) = mapT2 real (a,b)

-- pathsFrom :: (Coord, Elev) -> Path -> ElevMap -> [Path]
-- pathsFrom p@(c,e) visited es | isEnd e = [newVisited]
--                              | otherwise = concatMap (\nb -> pathsFrom nb newVisited es) safeNbs
--   where
--     safeNbs = filter (\nb -> nb `notElem` visited && (safe e $ snd nb)) $ nb4 c es
--     newVisited = p:visited

-- contains :: (Coord, Elev) -> H.Heap (H.Entry Int (Coord, Elev)) -> Bool
-- contains el heap = (==1) . H.size $ H.filter (\(H.Entry _ c) -> c == el) heap

-- updateKey :: (Ord a, Eq a, Ord b) => b -> a -> H.Heap (H.Entry a b) -> H.Heap (H.Entry a b)
-- updateKey val newKey heap = if contains val heap then H.map (\(H.Entry key v) -> if v == val then H.Entry newKey v else H.Entry key v) heap else 

-- getKey :: (Ord a, Eq a, Ord b) => b -> H.Heap (H.Entry a b) -> a
-- getKey val heap = H.priority . fst . fromJust . H.uncons $ H.filter (\(H.Entry _ v) -> v == val) heap

-- shortestPath :: (Coord, Elev) -> (Coord, Elev) -> ElevMap -> Path
-- shortestPath start end es = sp (H.singleton (H.Entry 0 start)) S.empty
--   where
--     sp :: H.Heap (H.Entry Int (Coord, Elev)) -> S.Set (Coord, Elev) -> M.Map (Coord, Elev) -> (Coord, Elev) Path
--     sp open closed preds | H.null open = error "no path found"
--                          | current == end = S.toList newClosed
--                          | otherwise = sp newOpen newClosed newPreds
--       where
--         (H.Entry currentG current, otherOpen) = fromJust $ H.uncons open
--         newClosed = S.insert current closed
--         safeNbs = filter (\nb@(c,e) -> (safe `on` snd $ current nb) && S.notMember nb newClosed) $ nb4 (fst current) es
--         (newOpen, newPreds) = foldl (\(os,ps) succ -> if contains succ os && g current os + 1 >= g succ os then (os, ps) else ) (open, preds) safeNbs
--         g = getKey

-- -- part1 :: String -> ElevMap
-- -- part1 input = subtract 1 . minimum . map length $ pathsFrom s [] es
-- -- part1 input = nb4 (fst s) es
-- -- part1 input = map ((flip move) (fst s)) directions
-- -- part1 input = length $ shortestPath s es
-- part1 input = shortestPath s es
--   where
--     es = mapCoords $ lines input
--     s = start es

-- part2 :: String -> String
-- part2 = const ""

main :: IO()
main = do
  args <- getArgs
  contents <- readFile $ head args
  
  putStrLn $ "Solution Day 12 Part 1: " ++ show (contents)
  putStrLn $ "Solution Day 12 Part 2: " ++ show (contents)

