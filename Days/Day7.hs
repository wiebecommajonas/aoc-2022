import Aoc
import qualified Data.Map.Strict as M (Map, lookup, unions, unionsWith, insert, empty, filter, foldl)
import Text.Parsec (try, optionMaybe, getPosition, eof, parse, many, manyTill, (<|>))
import Text.Parsec.Char (char, anyChar, string, digit, endOfLine, space)
import Text.Parsec.String (Parser)

data Cmd = Cd String | Ls deriving (Eq)
data Item = File Int String | Dir String deriving (Eq, Ord)
data Tree a = Branch a [Tree a] deriving (Eq)

isDir :: Item -> Bool
isDir (Dir _) = True
isDir _ = False

isCd :: Cmd -> Bool
isCd (Cd _) = True
isCd _ = False

leaf :: a -> Tree a
leaf a = Branch a []

root :: Tree a -> a
root (Branch r _) = r

-- compares roots of trees
(==*) :: Eq a => Tree a -> Tree a -> Bool
(==*) a b = root a == root b

-- .......
-- PARSING
-- .......

integer :: Parser Int
integer = many digit >>= return . read

ls :: Parser Cmd
ls = string "ls" >> endOfLine >> return Ls

cd :: Parser Cmd
cd = string "cd" >> space >> manyTill anyChar endOfLine >>= return . Cd

cdUp :: Parser Cmd
cdUp = string "$ cd .." >> endOfLine >> return (Cd "..")

cmd :: Parser Cmd
cmd = char '$' >> space >> (cd <|> ls)

file :: Parser Item
file = do
  size <- integer
  space
  name <- manyTill anyChar endOfLine
  return $ File size name

dir :: Parser Item
dir = string "dir" >> space >> manyTill anyChar endOfLine >>= return . Dir

item :: Parser Item
item = dir <|> file

dirTree :: Parser [Tree Item]
dirTree = do
  maybeEof <- optionMaybe eof
  if isJust maybeEof
    then return [] -- go up one level
    else do
      cdCmd <- cmd
      pos <- getPosition
      case cdCmd of
        Cd ".." -> return [] -- go up one level
        _ -> do
          cmd -- skip ls command
          items <- many item >>= return . map leaf . filter (not . isDir)
          let root = case cdCmd of
                       Cd dir -> Dir (dir ++ show pos) -- ensure unique Dir names
                       _ -> error "no root"
          next <- dirTree -- deeper level
          nnext <- dirTree -- same level
          return $ [Branch root (items ++ next)] ++ nnext

parseTree :: String -> Tree Item
parseTree input = case parse dirTree "" input of
                Left e -> error $ show e
                Right r -> head r

-- compute size of Dir (tree)
size :: Tree Item -> Int
size (Branch (File s _) _) = s
size (Branch _ bs) = sum . map size $ bs

-- maps every Dir to its size
-- needs unique Dir names
sizeMap :: Tree Item -> M.Map Item Int
sizeMap t = sm t M.empty
  where
    sm t@(Branch r@(Dir _) bs) m = M.insert r (size t) newM
      where newM = M.unionsWith (+) (map (\a -> sm a m) bs)
    sm _ m = m

part1 :: String -> Int
part1 = M.foldl (+) 0 . M.filter (<=100000) . sizeMap . parseTree

part2 :: String -> Int
part2 input = M.foldl min maxBound . M.filter (>=neededSpace) $ sizeMap tree
  where
    tree = parseTree input
    neededSpace = 30000000 - (70000000 - size tree)

main :: IO()
main = do
  args <- getArgs
  contents <- readFile $ head args
  
  putStrLn $ "Solution Day 7 Part 1: " ++ show (part1 contents)
  putStrLn $ "Solution Day 7 Part 2: " ++ show (part2 contents)

