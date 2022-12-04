import Aoc

data Action = Rock | Paper | Scissors deriving (Eq, Ord, Enum)
data TryTo = Win | Loose | Draw deriving (Eq, Ord)

actionPoints :: Action -> Int
actionPoints a = case a of
                  Rock -> 1
                  Paper -> 2
                  Scissors -> 3

toAction :: String -> Action
toAction x = case x of
              "X" -> Rock
              "A" -> Rock
              "Y" -> Paper
              "B" -> Paper
              "Z" -> Scissors
              "C" -> Scissors

win :: Action -> Action -> Bool
win Scissors Rock = True
win Paper Scissors = True
win Rock Paper = True
win _ _ = False

resultPoints :: Action -> Action -> Int
resultPoints a b | a==b = 3
                 | win a b = 6
                 | otherwise = 0

score :: [String] -> Int
score [a,b] = actionPoints y + resultPoints x y
  where
    x = toAction a
    y = toAction b

part1 :: String -> Int
part1 = sum . map (score . words) . lines

whatAction :: Action -> TryTo -> Action
whatAction a Draw = a
whatAction Scissors Win = Rock
whatAction a Win = succ a
whatAction Rock Loose = Scissors
whatAction a Loose = pred a

toTryTo :: String -> TryTo
toTryTo a = case a of
              "X" -> Loose
              "Y" -> Draw
              "Z" -> Win

tryscore :: [String] -> Int
tryscore [a,b] = actionPoints y + resultPoints x y
  where
    x = toAction a
    z = toTryTo b
    y = whatAction x z 

part2 :: String -> Int
part2 = sum . map (tryscore . words) . lines

main :: IO()
main = do
  args <- getArgs
  contents <- readFile $ head args
  
  putStrLn $ "Solution Day 2 Part 1: " ++ show (part1 contents)
  putStrLn $ "Solution Day 2 Part 2: " ++ show (part2 contents)

