module Aoc (module Prelude, module Aoc, module Data.Char, module Data.Maybe, module Data.List, module Data.List.Extra, module Data.List.Split, module System.Environment, module Control.Arrow) where
import           Prelude
import           Data.Char
import           Data.Maybe
import           Data.List          hiding (nub)
import           Data.List.Split    hiding (endBy, oneOf, sepBy)
import           Data.List.Extra    hiding (splitOn, merge, chunksOf, linesBy, wordsBy, lower, upper, split, nub)
import           System.Environment
import           Control.Arrow
import qualified Data.Map           as M
import qualified Data.Set           as S
import qualified Data.Vector        as V
import qualified Text.Parsec        as P
import qualified Text.Parsec.Char   as PC
import           Text.Parsec.String        (Parser)

-- .......
-- | LISTS
-- .......

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ [] = []
takeUntil p (x:xs) | p x = [x]
                   | otherwise = x : takeUntil p xs

insertAt :: Int -> a -> [a] -> [a]
insertAt i a as = take i as ++ a : (drop i as)

deleteAt :: Int -> [a] -> [a]
deleteAt i as = take i as ++ drop (i+1) as

replaceAt :: Int -> a -> [a] -> [a]
replaceAt i a as = insertAt i a . deleteAt i $ as

-- filters odd indices
odds :: [a] -> [a]
odds (_:xs) = evens xs
odds _ = []

-- filters even indices
evens :: [a] -> [a]
evens (x:xs) = x : odds xs
evens _ = []

-- count value in a list
count :: Eq a
    => a   -- ^ The value to look for
    -> [a] -- ^ The list to look in
    -> Int -- ^ The number of times the value is found in the list
count c = length . filter (==c)

histo :: Ord a => [a] -> [(a, Int)]
histo = map (head &&& length) . group . sort

tr :: Ord a
 => [a] -- ^ The "from" part of the mapping
 -> [a] -- ^ The "to" part of the mapping
 -> [a] -- ^ The original list
 -> [a] -- ^ The updated list after replacing "from" elements with their "to" counterparts
tr xs ys = map (\x -> fromMaybe x $ M.fromList (zip xs ys) M.!? x)

-- ........
-- | TUPLES
-- ........

mapT2 f (a,b) = (f a, f b)
mapT3 f (a,b,c) = (f a, f b, f c)
mapT4 f (a,b,c,d) = (f a, f b, f c, f d)

castT2 [a,b] = (a,b)
castT3 [a,b,c] = (a,b,c)
castT4 [a,b,c,d] = (a,b,c,d)
