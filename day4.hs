import           GridUtils

import           MUtils

type Warehouse = NeighbourMap Bool

parse :: [String] -> Warehouse
parse xs = map2 (== '@') xs |> gridFromList |> neighbourMapD

part1 :: [String] -> Int
part1 xs = indexes2 xs |> filter (getValue paper) |> map (getNeighbours paper) |> map2 snd |> map (count id)
    |> count (< 4)
  where
    paper = parse xs

countRemovablePaper :: [Point] -> (Warehouse, Int) -> Int
countRemovablePaper ps (w, n) = if n' == n then n else countRemovablePaper ps (w', n')
  where
    removable = ps |> filter (getValue w) |> filter (\p -> getNeighbours w p |> map snd |> count id |> (< 4))
    n' = n + length removable
    w' = foldr (changeValue (const False)) w removable

part2 :: [String] -> Int
part2 xs = countRemovablePaper (indexes2 xs) (parse xs, 0)

--changeValue :: (a->a) -> Point -> NeighbourMap a -> NeighbourMap a
test = [ "..@@.@@@@.", "@@@.@.@.@@", "@@@@@.@.@@", "@.@@@@..@.", "@@.@@@@.@@", ".@@@@@@@.@", ".@.@.@.@@@", "@.@@@.@@@@", ".@@@@@@@@.", "@.@.@@@.@." ]