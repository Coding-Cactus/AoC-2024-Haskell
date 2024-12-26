module Solutions (
  day1Part1,  day1Part2,
  day2Part1,  day2Part2,
  day3Part1,  day3Part2,
  day4Part1,  day4Part2,
  day5Part1,  day5Part2,
  day6Part1,  day6Part2,
  day7Part1,  day7Part2,
  day8Part1,  day8Part2,
  day9Part1,  day9Part2,
  day10Part1, day10Part2,
  day11Part1, day11Part2,
  day12Part1, day12Part2,
  day13Part1, day13Part2,
  day14Part1, day14Part2,
  day15Part1, day15Part2,
  day16Part1, day16Part2,
  day17Part1, day17Part2,
  day18Part1, day18Part2,
  day19Part1, day19Part2,
  day20Part1, day20Part2,
  day21Part1, day21Part2,
  day22Part1, day22Part2,
  day23Part1, day23Part2,
  day24Part1, day24Part2,
  day25Part1,
  Solution, inputfile, parse, runSolution
) where

import Data.List (sort, foldl', intercalate, maximumBy, isInfixOf, transpose)
import Data.List.Split (splitOn, chunksOf)

import Data.Char (digitToInt)

import Data.Composition ((.:))
import Text.Regex.TDFA ((=~))

import Data.Either (rights)

import Data.Maybe (isNothing, isJust, fromMaybe, catMaybes, mapMaybe)

import Data.Function (applyWhen, on)

import Data.Bits (xor, shiftL, shiftR, (.&.), (.|.))

import Data.Ratio (numerator, denominator)

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import qualified Data.Matrix as Matrix

import Utils


data Solution i o = Solution { inputfile :: String, parse :: [String] -> i, runSolution :: i -> o }

---------------------------------------------------------------------------------------------------------------------------
-- Day 1

day1Part1 :: Solution ([Int], [Int]) Int
day1Part2 :: Solution ([Int], [Int]) Int
day1Part1 = Solution "inputs/day1.txt" parse_day1 day1p1
day1Part2 = day1Part1 { runSolution = day1p2 }

parse_day1 :: [String] -> ([Int], [Int])
parse_day1 = unzip . map (listToPair . map read . words)

day1p1 :: ([Int], [Int]) -> Int
day1p1 (left, right) = sum $ zipWith (abs .: (-)) (sort left) (sort right)

day1p2 :: ([Int], [Int]) -> Int
day1p2 (left, right) = sum $ map (\e -> e * Map.findWithDefault 0 e counts) left
  where counts = tally right

---------------------------------------------------------------------------------------------------------------------------
-- Day 2

day2Part1 :: Solution [[Int]] Int
day2Part2 :: Solution [[Int]] Int
day2Part1 = Solution "inputs/day2.txt" parse_day2 day2p1
day2Part2 = day2Part1 { runSolution = day2p2 }

parse_day2 :: [String] -> [[Int]]
parse_day2 = map $ map read . words

day2p1 :: [[Int]] -> Int
day2p1 = count safe

day2p2 :: [[Int]] -> Int
day2p2 = count (\l -> any (safe . flip removeElemAt l) [0 .. length l - 1])

safe :: [Int] -> Bool
safe l = allSame (map (uncurry $ signum .: (-)) comps) && all (uncurry $ between 1 3 . abs .: (-)) comps
  where comps = zip l $ tail l

---------------------------------------------------------------------------------------------------------------------------
-- Day 3

day3Part1 :: Solution String Int
day3Part2 :: Solution String Int
day3Part1 = Solution "inputs/day3.txt" parse_day3 day3p1
day3Part2 = day3Part1 { runSolution = day3p2 }

parse_day3 :: [String] -> String
parse_day3 = concat

day3p1 :: String -> Int
day3p1 str = sum $ map mul $ str =~ "mul\\(([0-9]{1,3}),([0-9]{1,3})\\)"
  where
    mul (_ : x : [y]) = read x * read y
    mul _ = undefined

day3p2 :: String -> Int
day3p2 str = day3p1 $ concatMap (head . splitOn "don't()") $ splitOn "do()" str

---------------------------------------------------------------------------------------------------------------------------
-- Day 4

day4Part1 :: Solution (Map Coord Char) Int
day4Part2 :: Solution (Map Coord Char) Int
day4Part1 = Solution "inputs/day4.txt" parse_day4 day4p1
day4Part2 = day4Part1 { runSolution = day4p2 }

parse_day4 :: [String] -> Map Coord Char
parse_day4 = convert2DtoMap

day4p1 :: Map Coord Char -> Int
day4p1 grid = sum $ map xmas $ Map.keys grid
  where
    xmas (x, y) = count (checkCoord "XMAS" x y) [(dx, dy) | dx <- [-1 .. 1], dy <- [-1 .. 1]]

    checkCoord [] _ _ _ = True
    checkCoord (c : cs) x y (dx, dy) = c == Map.findWithDefault '.' (x, y) grid && checkCoord cs (x+dx) (y+dy) (dx, dy)


day4p2 :: Map Coord Char -> Int
day4p2 grid = count xmas $ Map.toList grid
  where
    xmas ((x, y), 'A') = any (all $ all (\(c, (dx, dy)) -> c == Map.findWithDefault '.' (x+dx, y+dy) grid)) combs
      where combs = [[zip c1 [(-1, -1), (1, 1)], zip c2 [(1, -1), (-1, 1)]] | c1 <- ["MS", "SM"], c2 <- ["MS", "SM"]]
    xmas _ = False

---------------------------------------------------------------------------------------------------------------------------
-- Day 5

day5Part1 :: Solution [PageNums] Int
day5Part2 :: Solution [PageNums] Int
day5Part1 = Solution "inputs/day5.txt" parse_day5 day5p1
day5Part2 = day5Part1 { runSolution = day5p2 }

data PageNum = PN (Set (String, String)) String
type PageNums = [PageNum]

getNum :: PageNum -> String
getNum (PN _ n) = n

instance Eq PageNum where
  (==) :: PageNum -> PageNum -> Bool
  (PN _ s1) == (PN _ s2) = s1 == s2

instance Ord PageNum where
  (<=) :: PageNum -> PageNum -> Bool
  (PN ordering s1) <= (PN _ s2) = s1 == s2 || (s1, s2) `Set.member` ordering

parse_day5 :: [String] -> [PageNums]
parse_day5 ls = map (map (PN ordering) . splitOn ",") $ filter (elem ',') ls
  where ordering = Set.fromList $ map (listToPair . splitOn "|") $ filter (elem '|') ls

day5p1 :: [PageNums] -> Int
day5p1 = sum . map (\nums -> read $ getNum $ nums !! (length nums `div` 2)) . filter (\nums -> nums == sort nums)

day5p2 :: [PageNums] -> Int
day5p2 = sum . map (\nums -> read $ getNum $ sort nums !! (length nums `div` 2)) . filter (\nums -> nums /= sort nums)

---------------------------------------------------------------------------------------------------------------------------
-- Day 6

day6Part1 :: Solution (Coord, Coord, Set Coord, Int) Int
day6Part2 :: Solution (Coord, Coord, Set Coord, Int) Int
day6Part1 = Solution "inputs/day6.txt" parse_day6 day6p1
day6Part2 = day6Part1 { runSolution = day6p2 }

parse_day6 :: [String] -> (Coord, Coord, Set (Int, Int), Int)
parse_day6 ls = (start, (0, -1), obstacles, length ls - 1)
  where
    grid = convert2DtoMap ls
    start = fst $ head $ filter ((==) '^' . snd) $ Map.toList grid
    obstacles = Set.fromList $ map fst $ filter ((==) '#' . snd) $ Map.toList grid

visitedSquares :: (Coord, Coord, Set Coord, Int) -> Set Coord
visitedSquares (coord, dir, obstacles, size) = Set.insert coord $
  if both (between 0 size) next_coord
    then visitedSquares (next_coord, next_dir dir, obstacles, size)
    else Set.empty
  where
    next_dir d = if (coord + d) `Set.member` obstacles then next_dir $ turnRight d else d
    next_coord = coord + next_dir dir

day6p1 :: (Coord, Coord, Set Coord, Int) -> Int
day6p1 = length . visitedSquares

day6p2 :: (Coord, Coord, Set Coord, Int) -> Int
day6p2 (init_coord, init_dir, obstacles, size) =
  count (isloop init_coord init_dir Set.empty . flip Set.insert obstacles) $
    Set.toList $ visitedSquares (init_coord, init_dir, obstacles, size)
  where
    isloop coord dir visited obs = both (between 0 size) coord && (
        (coord, dir) `Set.member` visited || isloop next_c (next_d dir) ((coord, dir) `Set.insert` visited) obs
      )
      where
        next_d d = if (coord + d) `Set.member` obs then next_d $ turnRight d else d
        next_c = coord + next_d dir

---------------------------------------------------------------------------------------------------------------------------
-- Day 7

day7Part1 :: Solution [(Int, [Int])] Int
day7Part2 :: Solution [(Int, [Int])] Int
day7Part1 = Solution "inputs/day7.txt" parse_day7 day7p1
day7Part2 = day7Part1 { runSolution = day7p2 }

parse_day7 :: [String] -> [(Int, [Int])]
parse_day7 = map (\l -> (read $ head $ splitOn ": " l, map read $ words $ head $ tail $ splitOn ": " l))

day7p1 :: [(Int, [Int])] -> Int
day7p1 = sum . map fst . filter (checkValid 0)
  where
    checkValid cur (total, []) = cur == total
    checkValid cur (total, x : xs) = cur <= total && (checkValid (cur + x) (total, xs) || checkValid (cur * x) (total, xs))

day7p2 :: [(Int, [Int])] -> Int
day7p2 = sum . map fst . filter (checkValid 0)
  where
    checkValid cur (total, []) = cur == total
    checkValid cur (total, x : xs) = cur <= total &&
      (checkValid (cur + x) (total, xs) || checkValid (cur * x) (total, xs) || checkValid (read $ show cur ++ show x) (total, xs))

---------------------------------------------------------------------------------------------------------------------------
-- Day 8

day8Part1 :: Solution (Int, [[Coord]]) Int
day8Part2 :: Solution (Int, [[Coord]]) Int
day8Part1 = Solution "inputs/day8.txt" parse_day8 day8p1
day8Part2 = day8Part1 { runSolution = day8p2 }

parse_day8 :: [String] -> (Int, [[Coord]])
parse_day8 ls = (
    length ls - 1,
    Map.elems $ foldl' (\m (coord, c) -> Map.insert c (coord : Map.findWithDefault [] c m) m) mempty $ filter ((/=) '.' . snd) $ Map.toList $ convert2DtoMap ls
  )

day8p1 :: (Int, [[Coord]]) -> Int
day8p1 (size, antennae) = length $ foldl' Set.union mempty $ map (Set.fromList . concatMap antinodes . pairs) antennae
  where
    antinodes (c1, c2) = filter (both $ between 0 size) [c1 + (c1 - c2), c2 + (c2 - c1)]

day8p2 :: (Int, [[Coord]]) -> Int
day8p2 (size, antennae) = length $ foldl' Set.union mempty $ map (Set.fromList . concatMap antinodes . pairs) antennae
  where
    antinodes (c1, c2) =
      takeWhile (both $ between 0 size) (map (\n -> c1 + (c1 - c2) * fromInteger n) [0..]) ++
      takeWhile (both $ between 0 size) (map (\n -> c2 + (c2 - c1) * fromInteger n) [0..])

---------------------------------------------------------------------------------------------------------------------------
-- Day 9

day9Part1 :: Solution ([(Int, Int)], [(Int, Int, Int, Bool)]) Int
day9Part2 :: Solution ([(Int, Int)], [(Int, Int, Int, Bool)]) Int
day9Part1 = Solution "inputs/day9.txt" parse_day9 day9p1
day9Part2 = day9Part1 { runSolution = day9p2 }

parse_day9 :: [String] -> ([(Int, Int)], [(Int, Int, Int, Bool)])
parse_day9 = snd . foldl' part (0, ([], [])) . zip [0..] . head
  where
    part (cum_i, (gaps, files)) (i, n)
      | even i = (cum_i + digitToInt n, (gaps, (cum_i, i `div` 2, digitToInt n, False) : files))
      | otherwise = (cum_i + digitToInt n, (gaps ++ [(cum_i, digitToInt n)], files))

moveFiles :: ([(Int, Int)], [(Int, Int, Int, Bool)]) -> (([(Int, Int)], [(Int, Int, Int, Bool)]) -> ([(Int, Int)], [(Int, Int, Int, Bool)])) -> Int
moveFiles (_, []) _ = 0
moveFiles (_, xs@((_, _, _, True) : _)) _ = foldl' (\c (i, fid, fcount, _) -> c + sum (map (fid *) [i .. i + fcount - 1])) 0 xs
moveFiles ([], xs) _ = moveFiles ([], map (\(i, fid, fcount, _) -> (i, fid, fcount, True)) xs) id
moveFiles gaps_and_files update_gaps_and_files = moveFiles (update_gaps_and_files gaps_and_files) update_gaps_and_files

day9p1 :: ([(Int, Int)], [(Int, Int, Int, Bool)]) -> Int
day9p1 gaps_and_files = moveFiles gaps_and_files update_gaps_and_files
  where
    update_gaps_and_files ((gapstart, gaplen):gs, (fstart, fid, flen, _):xs) = (new_gaps, new_files)
      where
        new_gaps
          | fstart < gapstart = (gapstart, gaplen) : gs
          | gaplen <= flen = gs
          | otherwise = (gapstart + flen, gaplen - flen) : gs
        new_files
          | fstart < gapstart = xs ++ [(fstart, fid, flen, True)]
          | flen <= gaplen = xs ++ [(gapstart, fid, flen, True)]
          | otherwise = (fstart, fid, flen - gaplen, False) : xs ++ [(gapstart, fid, gaplen, True)]
    update_gaps_and_files _ = undefined

day9p2 :: ([(Int, Int)], [(Int, Int, Int, Bool)]) -> Int
day9p2 gaps_and_files = moveFiles gaps_and_files update_gaps_and_files
  where
    update_gaps_and_files (gaps, (fstart, fid, flen, _):fs) = (new_gaps, new_files)
      where
        new_gaps = case find_gap_start gaps of
          Just gstart -> map (\(gs, gl) -> if gs == gstart then (gs + flen, gl - flen) else (gs, gl)) gaps
          _ -> gaps
        new_files = case find_gap_start gaps of
          Just gstart-> fs ++ [(gstart, fid, flen, True)]
          _ -> fs ++ [(fstart, fid, flen, True)]
        find_gap_start ((gstart, glen):gs) = if glen >= flen && gstart < fstart then Just gstart else find_gap_start gs
        find_gap_start [] = Nothing
    update_gaps_and_files _ = undefined

---------------------------------------------------------------------------------------------------------------------------
-- Day 10

day10Part1 :: Solution (Int, Map Coord Int) Int
day10Part2 :: Solution (Int, Map Coord Int) Int
day10Part1 = Solution "inputs/day10.txt" parse_day10 day10p1
day10Part2 = day10Part1 { runSolution = day10p2 }

parse_day10 :: [String] -> (Int, Map Coord Int)
parse_day10 ls = (length ls - 1, convert2DtoMap $ map (map digitToInt) ls)

trailends :: Map Coord Int -> Int -> Int -> Coord -> [Coord]
trailends grid size prev (x, y)
 | not (between 0 size x && between 0 size y) = []
 | prev + 1 /= cur = []
 | cur == 9 = [(x, y)]
 | otherwise = concatMap (trailends grid size cur . (+) (x, y)) directions
 where cur = grid Map.! (x, y)

day10p1 :: (Int, Map Coord Int) -> Int
day10p1 (size, grid) = sum $ map (length . Set.fromList . trailends grid size (-1) . fst) $ filter ((==) 0 . snd) $ Map.toList grid

day10p2 :: (Int, Map Coord Int) -> Int
day10p2 (size, grid) = sum $ map (length . trailends grid size (-1) . fst) $ filter ((==) 0 . snd) $ Map.toList grid

---------------------------------------------------------------------------------------------------------------------------
-- Day 11

day11Part1 :: Solution (Map Int Int) Int
day11Part2 :: Solution (Map Int Int) Int
day11Part1 = Solution "inputs/day11.txt" parse_day11 day11p1
day11Part2 = day11Part1 { runSolution = day11p2 }

parse_day11 :: [String] -> Map Int Int
parse_day11 = Map.fromList . map ((, 1) . read) . words . head

blink :: Int -> Map Int Int -> Int
blink 0 = sum . Map.elems
blink n = blink (n-1) . foldl' (\m (k, v) -> Map.insertWith (+) k v m) Map.empty . concatMap new_stones . Map.toList
  where
    new_stones (stone, c)
      | stone == 0 = [(1, c)]
      | even $ length $ show stone = [(read $ take middle $ show stone, c), (read $ drop middle $ show stone, c)]
      | otherwise = [(stone * 2024, c)]
      where middle = length (show stone) `div` 2

day11p1 :: Map Int Int -> Int
day11p1 = blink 25

day11p2 :: Map Int Int -> Int
day11p2 = blink 75

---------------------------------------------------------------------------------------------------------------------------
-- Day 12

day12Part1 :: Solution (Map Coord Char) Int
day12Part2 :: Solution (Map Coord Char) Int
day12Part1 = Solution "inputs/day12.txt" parse_day12 day12p1
day12Part2 = day12Part1 { runSolution = day12p2 }

parse_day12 :: [String] -> Map Coord Char
parse_day12 = convert2DtoMap

regions :: Map Coord Char -> [Set Coord]
regions grid = foldl' (\rs (coord, plant) -> if any (Set.member coord) rs then rs else dfs coord plant Set.empty : rs) [] $ Map.toList grid
  where
    dfs coord plant seen
      | coord `Set.member` seen || plant /= Map.findWithDefault '.' coord grid = seen
      | otherwise = foldl' (\acc new -> dfs new plant acc) (coord `Set.insert` seen) $ map (coord +) directions

day12p1 :: Map Coord Char -> Int
day12p1 = sum . map (\r -> length r * perimeter r) . regions
  where perimeter r = sum $ map (\p -> count (not . flip Set.member r . (+) p) directions) $ Set.toList r

day12p2 :: Map Coord Char -> Int
day12p2 grid = sum $ map (\r -> length r * side_count r) $ regions grid
  where
    side_count r = sum $ map sides $ Set.toList r
      where
        sides c = case count (flip Set.member r . (+) c) directions of
          0 -> 4
          1 -> 2
          _ -> count is_corner $ map (\d -> (get $ c+d, get (fst $ c+d, snd c), get (fst c, snd $ c+d))) diagonals
          where
            plant = grid Map.! c
            get crd = Map.findWithDefault '.' crd grid
            is_corner (dplant, adjplant1, adjplant2) =
              (dplant == plant && notElem plant [adjplant1, adjplant2]) ||
              (dplant /= plant && allSame (map (== plant) [adjplant1, adjplant2]))

---------------------------------------------------------------------------------------------------------------------------
-- Day 13

day13Part1 :: Solution [((Rational, Rational), (Rational, Rational), (Rational, Rational))] Int
day13Part2 :: Solution [((Rational, Rational), (Rational, Rational), (Rational, Rational))] Int
day13Part1 = Solution "inputs/day13.txt" parse_day13 day13p1
day13Part2 = day13Part1 { runSolution = day13p2 }

parse_day13 :: [String] -> [((Rational, Rational), (Rational, Rational), (Rational, Rational))]
parse_day13 = map parse . chunksOf 4
  where
    parse (l1 : l2 : l3 : _) = let
        (x1, y1) = listToPair $ map (fromIntegral . (read :: String -> Int)) $ tail $ head $ l1 =~ "X\\+([0-9]+), Y\\+([0-9]+)"
        (x2, y2) = listToPair $ map (fromIntegral . (read :: String -> Int)) $ tail $ head $ l2 =~ "X\\+([0-9]+), Y\\+([0-9]+)"
        (x3, y3) = listToPair $ map (fromIntegral . (read :: String -> Int)) $ tail $ head $ l3 =~ "X=([0-9]+), Y=([0-9]+)"
      in ((x1, y1), (x2, y2), (x3, y3))
    parse _ = undefined

day13p1 :: [((Rational, Rational), (Rational, Rational), (Rational, Rational))] -> Int
day13p1 = fromInteger . sum . map (\(a, b) -> numerator $ 3 * a + b) . filter (both ((==) 1 . denominator)) . rights . map (uncurry tickets . formMatrices)
  where
    tickets coeffs result = listToPair . Matrix.toList . (* result) <$> Matrix.inverse coeffs
    formMatrices ((x1, y1), (x2, y2), (x3, y3)) = (Matrix.fromLists [[x1, x2], [y1, y2]], Matrix.fromLists [[x3], [y3]])

day13p2 :: [((Rational, Rational), (Rational, Rational), (Rational, Rational))] -> Int
day13p2 = day13p1 . map (\(p1, p2, (x, y)) -> (p1, p2, (10000000000000 + x, 10000000000000 + y)))

---------------------------------------------------------------------------------------------------------------------------
-- Day 14

day14Part1 :: Solution [(Int, Int, Int, Int)] Int
day14Part2 :: Solution [(Int, Int, Int, Int)] Int
day14Part1 = Solution "inputs/day14.txt" parse_day14 day14p1
day14Part2 = day14Part1 { runSolution = day14p2 }

parse_day14 :: [String] -> [(Int, Int, Int, Int)]
parse_day14 = map (to4Tuple . map read . tail . head . flip (=~) "p=(-?[0-9]+),(-?[0-9]+) v=(-?[0-9]+),(-?[0-9]+)")
  where
    to4Tuple [x, y, dx, dy] = (x, y, dx, dy)
    to4Tuple _ = undefined

day14p1 :: [(Int, Int, Int, Int)] -> Int
day14p1 robots = product $ map (length . uncurry quadrant) quadrant_rules
  where
    endpoints = map (\(x, y, dx, dy) -> ((x + 100*dx) `mod` 101, (y + 100*dy) `mod` 103)) robots
    quadrant fx fy = filter (\(x, y) -> fx x && fy y) endpoints
    quadrant_rules = [(between 0 49, between 0 50), (between 51 101, between 0 50), (between 0 49, between 52 103), (between 51 101, between 52 103)]

day14p2 :: [(Int, Int, Int, Int)] -> Int
day14p2 = countSteps 0
  where
    countSteps n robots
      | any (\c -> all (flip Set.member coords . (+) c . (, 0)) [1..10]) coords = n
      | otherwise = countSteps (n+1) $ map (\(x, y, dx, dy) -> ((x + dx) `mod` 101, (y + dy) `mod` 103, dx, dy)) robots
      where coords = Set.fromList $ map (\(x, y, _, _) -> (x, y)) robots

---------------------------------------------------------------------------------------------------------------------------
-- Day 15

day15Part1 :: Solution (Coord, Map Coord Char, [Coord]) Int
day15Part2 :: Solution (Coord, Map Coord Char, [Coord]) Int
day15Part1 = Solution "inputs/day15.txt" parse_day15p1 day15p1
day15Part2 = day15Part1 { parse = parse_day15p2, runSolution = day15p2 }

parse_day15p1 :: [String] -> (Coord, Map Coord Char, [Coord])
parse_day15p1 ls = (
    fst $ head $ filter (\(_, c) -> c == '@') $ Map.toList $ convert2DtoMap $ filter (elem '#') ls,
    convert2DtoMap $ map (map (\c -> if c == '@' then '.' else c)) $ filter (elem '#') ls,
    map chevronToDirection $ concat $ filter (notElem '#') ls
  )

parse_day15p2 :: [String] -> (Coord, Map Coord Char, [Coord])
parse_day15p2 = parse_day15p1 . map (concatMap double)
  where
    double '#' = "##"
    double '.' = ".."
    double '@' = "@."
    double 'O' = "[]"
    double x = [x]

day15p1 :: (Coord, Map Coord Char, [Coord]) -> Int
day15p1 (_, grid, []) = sum $ map (\((x, y), _) -> x + 100 * y) $ filter (\(_, c) -> c == 'O') $ Map.toList grid
day15p1 (coord, grid, d:ds) = day15p1 (new_coord, new_grid, ds)
  where
    new_coord = if new_grid Map.! (coord + d) == '.' then coord + d else coord
    new_grid  = if can_move_box (coord + d) then move_box (coord + d) else grid
    can_move_box c
      | grid Map.! c == '.' = True
      | grid Map.! c == '#' = False
      | otherwise = can_move_box (c + d)
    move_box c
      | grid Map.! c `elem` ['.', '#'] = grid
      | otherwise = Map.insert c '.' $ Map.insert (c + d) 'O' $ move_box (c + d)

day15p2 :: (Coord, Map Coord Char, [Coord]) -> Int
day15p2 (_, grid, []) = sum $ map (\((x, y), _) -> x + 100 * y) $ filter (\(_, c) -> c == '[') $ Map.toList grid
day15p2 (coord, grid, d:ds) = day15p2 (new_coord, new_grid, ds)
  where
    new_coord = if new_grid Map.! (coord + d) == '.' then coord + d else coord
    new_grid  = if can_move_box (coord + d) then move_box (coord + d) grid else grid
    can_move_box c
      | grid Map.! c == '.' = True
      | grid Map.! c == '#' = False
      | snd d == 0 = can_move_box (c + 2*d)
      | otherwise = all can_move_box [c + d, c + d + if grid Map.! c == '[' then (1, 0) else (-1, 0)]
    move_box c g
      | g Map.! c `elem` ['.', '#'] = g
      | snd d == 0 =
          Map.insert c '.' $
          Map.insert (c + d) (grid Map.! c) $
          Map.insert (c + 2*d) (grid Map.! (c + d)) $
          move_box (c + 2*d) grid
      | otherwise = let (a, b) = (c, c + if grid Map.! c == '[' then (1, 0) else (-1, 0)) in
          Map.insert a '.' $
          Map.insert b '.' $
          Map.insert (a + d) (grid Map.! a) $
          Map.insert (b + d) (grid Map.! b) $
          move_box (a + d) $
          move_box (b + d) g

---------------------------------------------------------------------------------------------------------------------------
-- Day 16

day16Part1 :: Solution (Coord, Coord, Map Coord Char) Int
day16Part2 :: Solution (Coord, Coord, Map Coord Char) Int
day16Part1 = Solution "inputs/day16.txt" parse_day16 day16p1
day16Part2 = day16Part1 { runSolution = day16p2 }

parse_day16 :: [String] -> (Coord, Coord, Map Coord Char)
parse_day16 ls = (fst $ head $ Map.toList $ Map.filter ('S' ==) grid, fst $ head $ Map.toList $ Map.filter ('E' ==) grid, grid)
  where grid = convert2DtoMap ls

day16p1 :: (Coord, Coord, Map Coord Char) -> Int
day16p1 (start, end, grid) = mincost [(start, (1, 0), 0)] (fmap (\c -> (c, if c == 'S' then 0 else 9999999)) grid)
  where
    mincost [] c = snd $ c Map.! end
    mincost ((coord, dir, cost):cs) current = mincost (cs ++ adjacents) (foldl' (\new (crd, _, cst) -> Map.insert crd (grid Map.! crd, cst) new) current adjacents)
      where
        adjacents = filter use_neighbour (map (\(f, c) -> (coord + f dir, f dir, cost + 1000*c + 1)) [(id, 0), (turnLeft, 1), (turnRight, 1)])
        use_neighbour (crd, _, cst) = let (chr, c) = current Map.! crd in chr /= '#' && cst < c

day16p2 :: (Coord, Coord, Map Coord Char) -> Int
day16p2 (start, end, grid) = length $ seats (map (\(dir, _) -> (end, dir)) $ Map.toList $ Map.filter (path_mincost ==) end_dir_costs) Set.empty
  where
    seats [] s = s
    seats ((coord, dir):xs) s = seats (xs ++ map (\nc -> (nc, coord - nc)) (preds Map.! dir)) (coord `Set.insert` s)
      where (_, _, preds) = costs Map.! coord

    path_mincost = minimum $ Map.elems end_dir_costs :: Int
    (_, end_dir_costs, _) = costs Map.! end

    costs = mincost [(start, (1, 0), 0)] (fmap setup_cell grid)
      where setup_cell c = (c, Map.fromList $ if c == 'S' then zip directions [1000, 0, 1000, 2000] else [(d, 9999999) | d <- directions], Map.fromList [(d, []) | d <- directions])

    mincost [] c = c
    mincost ((coord, dir, cost):cs) current = mincost (cs ++ filter use_neighbour adjacents) (foldl' (\new (c, d, cst) -> Map.insert c (update_cell c d cst) new) current adjacents)
      where
        adjacents = map (\(f, c) -> (coord + f dir, f dir, cost + 1000*c + 1)) [(id, 0), (turnLeft, 1), (turnRight, 1)]
        use_neighbour (crd, _, cst) = let (chr, dircosts, _) = current Map.! crd in chr /= '#' && cst < minimum (Map.elems dircosts)

        update_cell crd s_dir s_cost = (chr, dir_costs, predecessors)
          where
            (chr, prev_dir_costs, prev_preds) = current Map.! crd
            dir_costs = Map.fromList $ map (\(f, c) -> (f s_dir, min (s_cost + c*1000) (prev_dir_costs Map.! f s_dir))) turns
            predecessors = Map.fromList $ map (\(f, c) -> (f s_dir, preds (s_cost + c*1000) (prev_dir_costs Map.! f s_dir) (prev_preds Map.! f s_dir))) turns
              where
                preds c prev_c prev_ps
                  | c < prev_c = [coord]
                  | c == prev_c = coord : prev_ps
                  | otherwise = prev_ps
            turns = [(id, 0), (turnLeft, 1), (turnRight, 1), (turnRight . turnRight, 2)]

---------------------------------------------------------------------------------------------------------------------------
-- Day 17

day17Part1 :: Solution (Int, Int, Int, [Int]) [Int]
day17Part2 :: Solution (Int, Int, Int, [Int]) Int
day17Part1 = Solution "inputs/day17.txt" parse_day17 day17p1
day17Part2 = day17Part1 { runSolution = day17p2 }

parse_day17 :: [String] -> (Int, Int, Int, [Int])
parse_day17 ls = (
    read $ splitOn ": " (head ls) !! 1,
    read $ splitOn ": " (ls !! 1) !! 1,
    read $ splitOn ": " (ls !! 2) !! 1,
    map read $ splitOn "," (splitOn ": " (ls !! 4) !! 1)
  )

day17p1 :: (Int, Int, Int, [Int]) -> [Int]
day17p1 (starta, startb, startc, instructions) = run (starta, startb, startc, 0, [])
  where
    run (a, b, c, ip, out)
      | ip >= length instructions = out
      | otherwise = run $
          case opcode of
            0 -> (a `div` 2^combo, b, c, ip + 2, out)
            1 -> (a, b `xor` operand, c, ip + 2, out)
            2 -> (a, combo `mod` 8, c, ip + 2, out)
            3 -> (a, b, c, if a == 0 then ip + 2 else operand, out)
            4 -> (a, b `xor` c, c, ip + 2, out)
            5 -> (a, b, c, ip + 2, out ++ [combo `mod` 8])
            6 -> (a, a `div` 2^combo, c, ip + 2, out)
            _ -> (a, b, a `div` 2^combo, ip + 2, out)
      where
        opcode = instructions !! ip
        operand = instructions !! (ip + 1)
        combo = case operand of
          4 -> a
          5 -> b
          6 -> c
          _ -> operand

day17p2 :: (Int, Int, Int, [Int]) -> Int
day17p2 (_, b, c, instructions) = startwith 0
  where
    startwith a = fromMaybe (startwith (a+1)) (form_num a)
    form_num a
      | res == instructions = Just a
      | res /= drop (length instructions - length res) instructions = Nothing
      | otherwise = foldl' (\acc n -> if isNothing acc && isJust n then n else acc) Nothing $ map (form_num . (+) (a*8)) [0..7]
      where res = day17p1 (a, b, c, instructions)

---------------------------------------------------------------------------------------------------------------------------
-- Day 18

day18Part1 :: Solution [Coord] Int
day18Part2 :: Solution [Coord] Coord
day18Part1 = Solution "inputs/day18.txt" parse_day18 day18p1
day18Part2 = day18Part1 { runSolution = day18p2 }

parse_day18 :: [String] -> [Coord]
parse_day18 = map (listToPair . map read . splitOn ",")

day18p1 :: [Coord] -> Int
day18p1 coords = expandFrontier [(0, 0)] [] 0 Set.empty
  where
    corrupted = Set.fromList $ take 1024 coords
    expandFrontier [] nextFrontier steps visited = if (70, 70) `elem` nextFrontier then steps + 1 else expandFrontier nextFrontier [] (steps + 1) visited
    expandFrontier (coord:cs) nextFrontier steps visited = expandFrontier cs (new ++ nextFrontier) steps (foldl' (flip Set.insert) visited new)
      where new = filter (\c -> both (between 0 70) c && c `Set.notMember` visited && c `Set.notMember` corrupted) $ map (coord +) directions

day18p2 :: [Coord] -> Coord
day18p2 coords = startat 1025
  where
    startat n = fromMaybe (startat (n+1)) (expandFrontier [(0, 0)] [] 0 Set.empty)
      where
        corrupted = Set.fromList $ take n coords
        expandFrontier [] nextFrontier steps visited
          | (70, 70) `elem` nextFrontier = Nothing
          | null nextFrontier = Just $ last $ take n coords
          | otherwise = expandFrontier nextFrontier [] (steps + 1 :: Int) visited
        expandFrontier (coord:cs) nextFrontier steps visited = expandFrontier cs (new ++ nextFrontier) steps (foldl' (flip Set.insert) visited new)
          where new = filter (\c -> both (between 0 70) c && c `Set.notMember` visited && c `Set.notMember` corrupted) $ map (coord +) directions

---------------------------------------------------------------------------------------------------------------------------
-- Day 19

day19Part1 :: Solution ([String], [String]) Int
day19Part2 :: Solution ([String], [String]) Int
day19Part1 = Solution "inputs/day19.txt" parse_day19 day19p1
day19Part2 = day19Part1 { runSolution = day19p2 }

parse_day19 :: [String] -> ([String], [String])
parse_day19 ls = (splitOn ", " $ head ls, drop 2 ls)

day19p1 :: ([String], [String]) -> Int
day19p1 (patterns, designs) = count possible designs
  where possible design = null design || any (\p -> p == take (length p) design && possible (drop (length p) design)) patterns

day19p2 :: ([String], [String]) -> Int
day19p2 (patterns, designs) = sum $ map options designs
  where
    options design = last dp
      where
        dp = map opt [0 .. length design]
        opt 0 = 1
        opt l = sum $ map (\p -> if length p <= l && p == take (length p) (drop (l - length p) design) then dp !! (l - length p) else 0) patterns

---------------------------------------------------------------------------------------------------------------------------
-- Day 20

day20Part1 :: Solution (Coord, Coord, Map Coord (Char, Int, Maybe Coord)) Int
day20Part2 :: Solution (Coord, Coord, Map Coord (Char, Int, Maybe Coord)) Int
day20Part1 = Solution "inputs/day20.txt" parse_day20 day20p1
day20Part2 = day20Part1 { runSolution = day20p2 }

parse_day20 :: [String] -> (Coord, Coord, Map Coord (Char, Int, Maybe Coord))
parse_day20 ls = let grid = convert2DtoMap ls in (
    fst $ head $ Map.toList $ Map.filter ('S' ==) grid,
    fst $ head $ Map.toList $ Map.filter ('E' ==) grid,
    fmap (\c -> (c, if c == 'S' then 0 else 9999999, Nothing)) grid
  )

computeCosts :: [(Coord, Int)] -> Map Coord (Char, Int, Maybe Coord) -> Map Coord (Char, Int, Maybe Coord)
computeCosts [] c = c
computeCosts ((coord, cost):cs) current =
  computeCosts (cs ++ adjacents) (foldl' (\new (crd, cst) -> Map.insert crd (fst3 $ current Map.! crd, cst, Just coord) new) current adjacents)
  where
    adjacents = filter use_neighbour (map (\d -> (coord + d, cost + 1)) directions)
    use_neighbour (crd, cst) = let (chr, c, _) = current Map.! crd in chr /= '#' && cst < c

countCheats :: Int -> Coord -> Map Coord (Char, Int, Maybe Coord) -> Int
countCheats jumps end grid = num_cheats (Just end) Set.empty
  where
    num_cheats Nothing cheats = length cheats
    num_cheats (Just crd) cheats = num_cheats nextcrd $
      foldl' (flip Set.insert) cheats [
        (crd, crd2) |
          dx <- [-jumps .. jumps], dy <- [-jumps + abs dx .. jumps - abs dx],
          let crd2 = crd + (dx, dy), let (chr, c, _) =  grid Map.! crd2,
          crd2 `Map.member` grid && chr /= '#' && c - (cheatcost + abs dx + abs dy) >= 100
      ]
      where
        (_, cheatcost, nextcrd) = grid Map.! crd

day20p1 :: (Coord, Coord, Map Coord (Char, Int, Maybe Coord)) -> Int
day20p1 (start, end, grid) = countCheats 2 end $ computeCosts [(start, 0)] grid

day20p2 :: (Coord, Coord, Map Coord (Char, Int, Maybe Coord)) -> Int
day20p2 (start, end, grid) = countCheats 20 end $ computeCosts [(start, 0)] grid

---------------------------------------------------------------------------------------------------------------------------
-- Day 21

day21Part1 :: Solution [String] Int
day21Part2 :: Solution [String] Int
day21Part1 = Solution "inputs/day21.txt" id day21p1
day21Part2 = day21Part1 { runSolution = day21p2 }

numKey :: Char -> Coord
numKey =  (Map.!) (Map.fromList [('9', (2, 0)), ('8', (1, 0)), ('7', (0, 0)), ('6', (2, 1)), ('5', (1, 1)), ('4', (0, 1)), ('3', (2, 2)), ('2', (1, 2)), ('1', (0, 2)), ('0', (1, 3)), ('A', (2, 3))])

dirKey :: Char -> Coord
dirKey = (Map.!) (Map.fromList [('^', (1, 0)), ('<', (0, 1)), ('v', (1, 1)), ('>', (2, 1)), ('A', (2, 0))])

minPresses :: Int -> [Char] -> Int
minPresses levels = fst . foldl' (\(s, start) end -> (s + min_press start end, end)) (0 , numKey 'A') . map numKey
  where
    min_press start end
      | dx == 0 = memoMinDirPresses vertBtn (abs dy) (dirKey 'A') levels + memoMinDirPresses (dirKey 'A') 1 vertBtn levels
      | dy == 0 = memoMinDirPresses horBtn  (abs dx) (dirKey 'A') levels + memoMinDirPresses (dirKey 'A') 1 horBtn levels
      | otherwise = minimum $ catMaybes [
          -- left/right, then up/down if it doesn't go over the hole
          if snd start == 3 && fst end == 0
            then Nothing
            else Just $ memoMinDirPresses horBtn  (abs dx) (dirKey 'A') levels
                      + memoMinDirPresses vertBtn (abs dy) horBtn levels
                      + memoMinDirPresses (dirKey 'A') 1 vertBtn levels,
          -- up/down, then left/right if it doesn't go over the hole
          if fst start == 0 && snd end == 3
            then Nothing
            else Just $ memoMinDirPresses vertBtn (abs dy) (dirKey 'A') levels
                      + memoMinDirPresses horBtn  (abs dx) vertBtn levels
                      + memoMinDirPresses (dirKey 'A') 1 horBtn levels
        ]
      where
        (dx, dy) = end - start
        vertBtn = if dy > 0 then dirKey 'v' else dirKey '^'
        horBtn = if dx > 0 then dirKey '>' else dirKey '<'

memoMinDirPresses :: Coord -> Int -> Coord -> Int -> Int
memoMinDirPresses = memo4 minDirPresses

minDirPresses :: Coord -> Int -> Coord -> Int -> Int
minDirPresses target clicks start level
  | level == 1 = abs dx + abs dy + clicks
  | dx == 0 = memoMinDirPresses vertBtn (abs dy) (dirKey 'A') (level - 1) + memoMinDirPresses (dirKey 'A') clicks vertBtn (level - 1)
  | dy == 0 = memoMinDirPresses horBtn  (abs dx) (dirKey 'A') (level - 1) + memoMinDirPresses (dirKey 'A') clicks horBtn (level - 1)
  | otherwise = minimum $ catMaybes [
          -- left/right first, then up/down if it doesn't go over the hole
          if snd start == 0 && fst target == 0
            then Nothing
            else Just $ memoMinDirPresses horBtn  (abs dx) (dirKey 'A') (level - 1)
                      + memoMinDirPresses vertBtn (abs dy) horBtn (level - 1)
                      + memoMinDirPresses (dirKey 'A') clicks vertBtn (level - 1),
          -- up/down first, then left/right if it doesn't go over the hole
          if fst start == 0 && snd target == 0
            then Nothing
            else Just $ memoMinDirPresses vertBtn (abs dy) (dirKey 'A') (level - 1)
                      + memoMinDirPresses horBtn  (abs dx) vertBtn (level - 1)
                      + memoMinDirPresses (dirKey 'A') clicks horBtn (level - 1)
        ]
  where
    (dx, dy) = target - start
    vertBtn = if dy > 0 then dirKey 'v' else dirKey '^'
    horBtn = if dx > 0 then dirKey '>' else dirKey '<'

day21p1 :: [String] -> Int
day21p1 = sum . map (\code -> minPresses 2 code * read (take 3 code))

day21p2 :: [String] -> Int
day21p2 = sum . map (\code -> minPresses 25 code * read (take 3 code))

---------------------------------------------------------------------------------------------------------------------------
-- Day 22

day22Part1 :: Solution [Int] Int
day22Part2 :: Solution [Int] Int
day22Part1 = Solution "inputs/day22.txt" parse_day22 day22p1
day22Part2 = day22Part1 { runSolution = day22p2 }

parse_day22 :: [String] -> [Int]
parse_day22 = map read

secretNumbers :: Int -> [Int]
secretNumbers s0 = s0 : secretNumbers s1
  where
    i1 = s0 `xor` (s0 `shiftL` 6  `mod` 16777216)
    i2 = i1 `xor` (i1 `shiftR` 5  `mod` 16777216)
    s1 = i2 `xor` (i2 `shiftL` 11 `mod` 16777216)

day22p1 :: [Int] -> Int
day22p1 = sum . map (flip (!!) 2000 . secretNumbers)

day22p2 :: [Int] -> Int
day22p2 = maximum . Map.elems . foldl' (Map.unionWith (+)) Map.empty . map seqs
  where
    diffs seed = zipWith (\a b -> (b `mod` 10, b `mod` 10 - a `mod` 10)) (take 2001 $ secretNumbers seed) (tail $ take 2001 $ secretNumbers seed)
    seqs = foldl' (\m (s, banan) -> applyWhen (s `Map.notMember` m) (Map.insert s banan) m) Map.empty . groups4 . diffs

    groups4 xs@((_, a) : (_, b) : (_, c) : (banan, d) : _) = ((a, b, c, d), banan) : groups4 (tail xs)
    groups4 _ = []

---------------------------------------------------------------------------------------------------------------------------
-- Day 23

day23Part1 :: Solution (Map String (Set String)) Int
day23Part2 :: Solution (Map String (Set String)) String
day23Part1 = Solution "inputs/day23.txt" parse_day23 day23p1
day23Part2 = day23Part1 { runSolution = day23p2 }

parse_day23 :: [String] -> Map String (Set String)
parse_day23 = foldl' add_edge Map.empty . map (listToPair . splitOn "-")
  where
    add_edge g (u, v) = Map.alter (add_vertex u) v $ Map.alter (add_vertex v) u g
    add_vertex v Nothing = Just $ Set.singleton v
    add_vertex v (Just vs) = Just $ Set.insert v vs

day23p1 :: Map String (Set String) -> Int
day23p1 g = length $ foldl' Set.union Set.empty $ map triangles $ filter ((==) 't' . head . fst) $ Map.toList g
  where
    triangles (v, neighbours) = foldl' Set.union Set.empty $ map common_ns $ Set.toList neighbours
      where common_ns n1 = Set.fromList $ map (\n2 -> sort [v, n1, n2]) $ filter (n1 /=) $ Set.toList $ neighbours `Set.intersection` (g Map.! n1)

maxClique :: Set String -> Set String -> Set String -> Map String (Set String) -> Set String
maxClique clique remaining_v exclude_v g
  | null remaining_v && null exclude_v = clique
  | otherwise = fst $ foldl' extend (Set.empty, (remaining_v, exclude_v)) remaining_v
  where
    extend (max_c, (rem_v, exc_v)) v = (next_c, (v `Set.delete` rem_v, v `Set.insert` exc_v))
      where next_c = maximumBy (compare `on` length) [max_c, maxClique (v `Set.insert` clique) (rem_v `Set.intersection` (g Map.! v)) (exc_v `Set.intersection` (g Map.! v)) g]

day23p2 :: Map String (Set String) -> String
day23p2 g = intercalate "," $ sort $ Set.toList $ maxClique Set.empty (Map.keysSet g) Set.empty g

---------------------------------------------------------------------------------------------------------------------------
-- Day 24

day24Part1 :: Solution (Map String Int, Map String (String, String, String)) Int
day24Part2 :: Solution (Map String Int, Map String (String, String, String)) String
day24Part1 = Solution "inputs/day24.txt" parse_day24 day24p1
day24Part2 = day24Part1 { runSolution = day24p2 }

parse_day24 :: [String] -> (Map String Int, Map String (String, String, String))
parse_day24 ls = (
    foldl' (\m (k, v) -> Map.insert k (fromEnum $ v == "1") m) Map.empty $ map (listToPair . splitOn ": ") $ filter (":" `isInfixOf`) ls,
    foldl' (\m (inp, out) -> Map.insert out inp m) Map.empty $ map ((\(inp, out) -> (listToTriple $ words inp, out)) . listToPair . splitOn " -> ") $ filter ("->" `isInfixOf`) ls
  )

day24p1 :: (Map String Int, Map String (String, String, String)) -> Int
day24p1 (values, expressions) = sum $ zipWith (\n wire -> 2^n * eval wire) [(0::Int)..] $ takeWhile (`Map.member` expressions) [ (if n < 10 then "z0" else "z") ++ show n | n <- [0..] :: [Int] ]
  where
    eval wire
      | wire `Map.member` values = values Map.! wire
      | otherwise = case expressions Map.! wire of
          (i1, "OR",  i2) -> eval i1  .|.  eval i2
          (i1, "AND", i2) -> eval i1  .&.  eval i2
          (i1, "XOR", i2) -> eval i1 `xor` eval i2
          _ -> undefined

day24p2 :: (Map String Int, Map String (String, String, String)) -> String
day24p2 (values, expressions) = intercalate "," $ sort $ concatMap bad $ Map.toList expressions
  where
    bad (out, (in1, op, in2))
      | all (\(i1, _, i2) -> out `notElem` [i1, i2]) expressions =
          if out == "z45" then
            [out | op /= "OR"]
          else
            if op /= "XOR" || any (\i -> i `Map.member` values && tail i /= tail out) [in1, in2] then
              [out]
            else
              mapMaybe check_inputs [in1, in2]
      | op == "OR" = mapMaybe (\i -> let (_, o, _) = expressions Map.! i in if o /= "AND" then Just i else Nothing) [in1, in2]
      | otherwise = []
      where
        check_inputs inp
          | inp `Map.notMember` expressions = Nothing
          | any (`Map.member` values) [i1, i2] = if o /= "XOR" && ["x00", "y00"] /= sort [i1, i2] then Just inp else Nothing
          | otherwise = if o /= "OR" && inp `Map.notMember` values then Just inp else Nothing
          where (i1, o, i2) = expressions Map.! inp

---------------------------------------------------------------------------------------------------------------------------
-- Day 25

day25Part1 :: Solution ([[Int]], [[Int]]) Int
day25Part1 = Solution "inputs/day25.txt" parse_day25 day25p1

parse_day25 :: [String] -> ([[Int]], [[Int]])
parse_day25 ls = (to_levels $ filter (elem '#' . head) grids, to_levels $ filter (notElem '#' . head) grids)
  where
    to_levels = map (map (count ('#' ==)) . transpose)
    grids = map (splitOn "\n") $ splitOn "\n\n" $ intercalate "\n" ls

day25p1 :: ([[Int]], [[Int]]) -> Int
day25p1 (locks, keys) = length [ (lock, key) | lock <- locks, key <- keys, and $ zipWith ((>=) 7 .: (+)) lock key ]
