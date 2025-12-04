import           Data.List
import           Data.Maybe ( fromJust )

import           MUtils

joltage :: Int -> [Int] -> Int
joltage 0 _ = 0
joltage n xs = best * 10 ^ (n - 1) + joltage (n - 1) (drop (elemIndex best xs |> fromJust |> (+ 1)) xs)
  where
    best = maximum (take (length xs - (n - 1)) xs)

part1 :: [String] -> Int
part1 xs = map2 singleton xs |> map2 readInt |> map (joltage 2) |> sum

part2 :: [String] -> Int
part2 xs = map2 singleton xs |> map2 readInt |> map (joltage 12) |> sum

test = [ "987654321111111", "811111111111119", "234234234234278", "818181911112111" ]