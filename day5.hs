import           Data.List ( partition )

import           MUtils

type Range = (Integer, Integer)

parse :: [String] -> ([Range], [Integer])
parse xs = splitOn "" xs |> t2fromList |> mapFst (map (t2fromList . map read . splitOn '-')) |> mapSnd (map read)

inRange :: [Range] -> Integer -> Bool
inRange [] _ = False
inRange ((l, u) : xs) n = (n >= l && n <= u) || inRange xs n

part1 :: [String] -> Int
part1 xs = let (r, i) = parse xs in count (inRange r) i

addRange :: [Range] -> Range -> [Range]
addRange rs r@(l, u) = (l', u') : other
  where
    (intersecting, other) = partition (\(l', u') -> u' >= l && l' <= u) rs
    l' = map fst intersecting |> (l :) |> minimum
    u' = map snd intersecting |> (u :) |> maximum

part2 :: [String] -> Integer
part2 xs = parse xs |> fst |> foldr (flip addRange) [] |> map (\(a, b) -> b - a + 1) |> sum

test = [ "3-5", "10-14", "16-20", "12-18", "", "1", "5", "8", "11", "17", "32" ]
