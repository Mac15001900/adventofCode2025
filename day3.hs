import           Data.List

import           MUtils

joltage :: [Int] -> Int
joltage [] = 0
joltage [x] = 0
joltage (x : xs) = max (x * 10 + maximum xs) (joltage xs)

part1 :: [String] -> Int
part1 xs = map2 singleton xs |> map2 readInt |> map joltage |> sum

test = [ "987654321111111", "811111111111119", "234234234234278", "818181911112111" ]