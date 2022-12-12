import Aoc hiding (round)
import Prelude hiding (round)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (digit, string, char, space, spaces, endOfLine)
import Text.Parsec (parse, many, manyTill, sepBy, (<|>))

type Worry = Int
type Test = Int
type Relief = Worry -> Worry

data Symbol = Literal Int | Old deriving (Show, Eq)
data Op = Mul | Add deriving (Show, Eq)
data Operation = Operation Op Symbol Symbol deriving (Show, Eq)
data Monkey = Monkey { mName        :: Int
                     , mItems       :: [Worry]
                     , mOperation   :: Operation
                     , mTest        :: Int
                     , mThrowTo     :: (Int, Int)
                     , mInspections :: Int} deriving (Show, Eq)

ignore :: Parser a -> b -> Parser b
ignore a b = a >> return b

integer :: Parser Int
integer = many digit >>= return . read

int :: Parser Int
int = many digit >>= return . read

name :: Parser Int
name = string "Monkey" >> space >> int >>= ignore (char ':' >> endOfLine)

items :: Parser [Worry]
items = spaces >> string "Starting items: " >> integer `sepBy` (string ", ") >>= ignore endOfLine

pMul :: Parser Op
pMul = char '*' >> return Mul
pAdd :: Parser Op
pAdd = char '+' >> return Add
pOp :: Parser Op
pOp = pMul <|> pAdd

old :: Parser Symbol
old = string "old" >> return Old

literal :: Parser Symbol
literal = integer >>= return . Literal

symbol :: Parser Symbol
symbol = old <|> literal

operation :: Parser Operation
operation = do
  spaces
  string "Operation: new = "
  x <- symbol
  space
  op <- pOp
  space
  y <- symbol
  endOfLine
  return $ Operation op x y

test :: Parser Int
test = spaces >> string "Test: divisible by " >> integer >>= ignore endOfLine

throwTo :: Parser (Int, Int)
throwTo = do
  spaces
  string "If true: throw to monkey "
  name1 <- int
  endOfLine
  spaces
  string "If false: throw to monkey "
  name2 <- int
  endOfLine
  return (name1, name2)

monkey :: Parser Monkey
monkey = do
  n <- name
  is <- items
  op <- operation
  t <- test
  tt <- throwTo
  return $ Monkey {mName = n, mItems = is, mOperation = op, mTest = t, mThrowTo = tt, mInspections = 0}

monkeys :: String -> [Monkey]
monkeys input = case parse (monkey `sepBy` endOfLine) "" input of
                 Left e -> error $ show e
                 Right s -> s

getInt :: Int -> Symbol -> Int
getInt old Old = old
getInt _ (Literal n) = n

doOperation :: Int -> Operation -> Int
doOperation old (Operation Mul a b) = getInt old a * getInt old b
doOperation old (Operation Add a b) = getInt old a + getInt old b

doTest :: Test -> Worry -> Bool
doTest test w = w `rem` test == 0

inspect :: Relief -> Monkey -> Worry -> (Worry, Bool)
inspect relief m w = (new, doTest (mTest m) new)
  where
    new = relief $ doOperation w (mOperation m)

mAddItem :: Monkey -> Worry -> Monkey
mAddItem m w = Monkey (mName m) (items) (mOperation m) (mTest m) (mThrowTo m) (mInspections m)
  where items = mItems m `snoc` w

mDropItem :: Monkey -> Monkey
mDropItem m = Monkey (mName m) (drop 1 $ mItems m) (mOperation m) (mTest m) (mThrowTo m) (mInspections m + 1)

throw :: Monkey -> Worry -> Bool -> [Monkey] -> [Monkey]
throw m w b ms = replaceAt mThis mThisNew $ replaceAt mNext mNew ms
  where
    mThis = mName m
    mThisNew = mDropItem m
    mNext = (if b then fst else snd) (mThrowTo m)
    mNew = mAddItem (ms !! mNext) w

turn1 :: Relief -> Monkey -> Worry -> [Monkey] -> [Monkey]
turn1 r m w ms = throw m wNew test ms
  where
    (wNew, test) = inspect r m w

turn :: Relief -> Monkey -> [Monkey] -> [Monkey]
turn _ (Monkey _ [] _ _ _ _) ms = ms
turn r m@(Monkey _ (i:is) _ _ _ _) ms = turn r (mDropItem m) (turn1 r m i ms)

round :: Relief -> [Monkey] -> [Monkey]
round r ms = helper 0 ms
  where
    helper n ms | n == length ms = ms
                | otherwise = helper (n+1) (turn r (ms !! n) ms)

mBusiness :: [Monkey] -> Int
mBusiness ms = product . take 2 . reverse . sort $ map mInspections ms 

addItems :: [Monkey] -> Int
addItems = sum . map (length . mItems)

relief1 :: Worry -> Worry
relief1 w = w `div` 3

part1 :: String -> Int
part1 = mBusiness . last . take 21 . iterate (round relief1) . monkeys

relief2 :: Int -> Worry -> Worry
relief2 = flip mod

-- part2 :: String -> String
part2 input = mBusiness . last . take 10001 . iterate (round (relief2 bigMod)) $ ms
  where
    ms = monkeys input
    bigMod = product $ map mTest ms

main :: IO()
main = do
  args <- getArgs
  contents <- readFile $ head args
  
  putStrLn $ "Solution Day 11 Part 1: " ++ show (part1 contents)
  putStrLn $ "Solution Day 11 Part 2: " ++ show (part2 contents)

