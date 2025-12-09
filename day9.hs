import           Data.List

import           MUtils

type Point = (Int, Int)
type Edge = (Point, Point)
type Area = (Point, Point)

parse :: [String] -> [Point]
parse = map (\l -> splitOn ',' l |> map readInt |> t2fromList)

calcArea :: Area -> Int
calcArea ((x1, y1), (x2, y2)) = (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)

part1 :: [String] -> Int
part1 xs = parse xs |> combinationsSelf |> map calcArea |> maximum

--There are funny edge cases where this doesn't quite work, but they don't happen in the input. I might still fix it at some point when I feel like it.
isInside :: [Edge] -> Point -> Bool
isInside es (x, y) = any (\((x1, y1), (x2, y2)) -> x == x1 && x == x2 && inBounds (y1, y2) y || y == y1 && y == y2 && inBounds (x1, x2) x) es
    || (count (\((x1, y1), (x2, y2)) -> y1 == y2 && y1 < y && inBounds (x1, x2) x) es |> odd)

--For each edge check if the coordinate that changes is not entirely outside of the area and the coordinate that doesn't change is within the area
anyEdgeInArea :: [Edge] -> Area -> Bool
anyEdgeInArea [] _ = False
anyEdgeInArea (((ex1, ey1), (ex2, ey2)) : es) a@((ax1, ay1), (ax2, ay2))
    | ey1 == ey2 = inBounds (ay1, ay2) ey1 && not (eux < min ax1 ax2 || elx > max ax1 ax2) || anyEdgeInArea es a
    | otherwise  = inBounds (ax1, ax2) ex1 && not (euy < min ay1 ay2 || ely > max ay1 ay2) || anyEdgeInArea es a
  where
    [elx, eux] = sort [ ex1, ex2 ] -- [Edge Lower X, Edge Upper X]
    [ely, euy] = sort [ ey1, ey2 ] -- [Edge Lower Y, Edge Upper Y]

shrinkArea :: Area -> Area
shrinkArea ((x1, y1), (x2, y2)) = ((min x1 x2 + 1, min y1 y2 + 1), (max x1 x2 - 1, max y1 y2 - 1))

midPoint :: Area -> Point
midPoint ((x1, y1), (x2, y2)) = ((x1 + x2) `div` 2, (y1 + y2) `div` 2)

part2 :: [String] -> Int
part2 xs = combinationsSelf points |> sortOn calcArea |> reverse |> filter (not . anyEdgeInArea edges . shrinkArea)
    |> filter (isInside edges . midPoint) |> head |> calcArea
  where
    points = parse xs
    edges = zip points (tail points ++ [ head points ])

test = [ "7,1", "11,1", "11,7", "9,7", "9,5", "2,5", "2,3", "7,3" ]