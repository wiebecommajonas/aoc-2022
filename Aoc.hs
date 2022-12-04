module Aoc (module Prelude, module Data.Char, module Data.Maybe, module Data.List, module Data.List.Extra, module Data.List.Split, module System.Environment, module Control.Arrow) where
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
  
  count :: Eq a
      => a   -- ^ The value to look for
      -> [a] -- ^ The list to look in
      -> Int -- ^ The number of times the value is found in the list
  count c = length . filter (==c)

  histo :: Ord a => [a] -> [(a, Int)]
  histo = map (head &&& length) . group . sort

  -- |The 'tr' function translates lists according to a given mapping.
  --
  -- >>> tr "LR" "01" "LALR"
  -- "0A01"
  --
  tr :: Ord a
   => [a] -- ^ The "from" part of the mapping
   -> [a] -- ^ The "to" part of the mapping
   -> [a] -- ^ The original list
   -> [a] -- ^ The updated list after replacing "from" elements with their "to" counterparts
  tr xs ys = map (\x -> fromMaybe x $ M.fromList (zip xs ys) M.!? x)