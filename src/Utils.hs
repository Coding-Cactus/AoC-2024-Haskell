module Utils where

import Data.Composition ((.:))
import Data.List (foldl')
import Data.MemoTrie (mup, memo3, HasTrie)
import Data.Map (Map)
import qualified Data.Map as Map
import Prelude hiding (Left, Right)

listToPair :: [a] -> (a, a)
listToPair [] = undefined
listToPair [e] = (e, e)
listToPair (x : y : _) = (x, y)

listToTriple :: [a] -> (a, a, a)
listToTriple [x, y, z] = (x, y, z)
listToTriple _ = undefined

tally :: Ord a => [a] -> Map a Int
tally = foldl' (flip $ Map.alter inc) Map.empty
  where
    inc Nothing = Just 1
    inc (Just x) = Just $ x + 1

between :: Int -> Int -> Int -> Bool
between l u num = num >= l && num <= u

removeElemAt :: Int -> [a] -> [a]
removeElemAt _ [] = []
removeElemAt i xs = take i xs ++ drop (i+1) xs

allSame :: (Eq a) => [a] -> Bool
allSame [] = True
allSame (x:xs) = all (x ==) xs

count :: (a -> Bool) -> [a] -> Int
count = length .: filter

convert2DtoMap :: [[a]] -> Map (Int, Int) a
convert2DtoMap = Map.fromList . concat . zipWith (\y -> zipWith (\x -> (,) (x, y)) [0..]) [0..]


type Coord = (Int, Int)

instance Num Coord where
  (+) :: Coord -> Coord -> Coord
  (x1, y1) + (x2, y2) = (x1 + x2, y1 + y2)

  (*) :: Coord -> Coord -> Coord
  (x1, y1) * (x2, y2) = (x1 * x2, y1 * y2)

  negate :: Coord -> Coord
  negate (x, y) = (-x, -y)

  abs :: Coord -> Coord
  abs (x, y) = (abs x, abs y)

  fromInteger :: Integer -> Coord
  fromInteger n = (fromInteger n, fromInteger n)

  signum :: Coord -> Coord
  signum (x, y) = (signum x, signum y)

directions :: [Coord]
directions = [(0, -1), (1, 0), (0, 1), (-1, 0)]

diagonals :: [Coord]
diagonals = [(1, -1), (1, 1), (-1, 1), (-1, -1)]

chevronToDirection :: Char -> Coord
chevronToDirection '^' = (0, -1)
chevronToDirection '>' = (1, 0)
chevronToDirection 'v' = (0, 1)
chevronToDirection '<' = (-1, 0)
chevronToDirection _ = undefined

turnRight :: Coord -> Coord
turnRight (0, -1) = (1, 0)
turnRight (1, 0) = (0, 1)
turnRight (0, 1) = (-1, 0)
turnRight (-1, 0) = (0, -1)
turnRight _ = undefined

turnLeft :: Coord -> Coord
turnLeft (0, -1) = (-1, 0)
turnLeft (-1, 0) = (0, 1)
turnLeft (0, 1) = (1, 0)
turnLeft (1, 0) = (0, -1)
turnLeft _ = undefined

both :: (a -> Bool) -> (a, a) -> Bool
both f (a, b) = f a && f b

pairs :: Ord b => [b] -> [(b, b)]
pairs ps = [(p1, p2) | p1 <- ps, p2 <- ps, p1 < p2]

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

mid3 :: (a, b, c) -> b
mid3 (_, b, _) = b

lst3 :: (a, b, c) -> c
lst3 (_, _, c) = c

memo4 :: (HasTrie r, HasTrie s, HasTrie t, HasTrie u) => (r -> s -> t -> u -> a) -> (r -> s -> t -> u -> a)
memo4 = mup memo3
