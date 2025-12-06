import           Data.Char ( isDigit )
import           Data.List

import           MUtils

type Problem = (Char, [Int])

parse :: [String] -> [Problem]
parse xs = map (splitOn ' ') xs |> map (filter (not . null)) |> transpose |> map (\l -> (last l |> head, init l |> map readInt))

evaluate :: Problem -> Int
evaluate ('*', xs) = product xs
evaluate ('+', xs) = sum xs

part1 :: [String] -> Int
part1 xs = parse xs |> map evaluate |> sum

parse2 :: [String] -> [Problem]
parse2 xs = map reverse xs |> transpose |> split (all (== ' ')) |> filter3 (/= ' ') |> map (\pr -> (last pr |> last, filter2 isDigit pr |> map readInt))

part2 :: [String] -> Int
part2 xs = parse2 xs |> map evaluate |> sum

test = [ "123 328  51 64 ", " 45 64  387 23 ", "  6 98  215 314", "*   +   *   +  " ]