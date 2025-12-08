import           Data.List
import           Data.Maybe ( fromJust )

import           MUtils

runStep :: String -> String -> String
runStep [] _ = []
runStep prev next = intersect (indexesWhere (== '|') prev) (indexesWhere (== '^') next) |> map (\n -> [ n - 1, n + 1 ]) |> concat
    |> (++ indexesWhere (== '|') prev) |> uniqueSet |> intersect (indexesWhere (== '.') next) |> foldr (\i -> setElement i '|') next

runBeam :: [String] -> [String]
runBeam [x] = []
runBeam (p : n : xs) = let n' = runStep p n in n' : runBeam (n' : xs)

countSplits :: [String] -> Int
countSplits xs = indexes2 xs |> count (\(x, y) -> xs !! y !! x == '^' && xs !! (y - 1) !! x == '|')

part1 :: [String] -> Int
part1 xs = replace2 'S' '|' xs |> runBeam |> countSplits

updateTimelines :: String -> String -> [Int] -> [Int]
updateTimelines _ _ [t1, t2] = []
updateTimelines (p1 : p2 : p3 : ps) (n1 : n2 : n3 : ns) (t1 : t2 : t3 : ts) =
    ((if n1 == '^' && p1 == '|' then t1 else 0) + (if p2 == '|' then t2 else 0) + (if n3 == '^' && p3 == '|' then t3 else 0))
    : updateTimelines (p2 : p3 : ps) (n2 : n3 : ns) (t2 : t3 : ts)
updateTimelines p n t' = error (show p)

runBeam2 :: [Int] -> [String] -> Int
runBeam2 t [x] = sum t
runBeam2 t (p : n : xs) = runBeam2 t' (n' : xs)
  where
    n' = runStep p n
    t' = updateTimelines ('.' : p ++ [ '.' ]) ('.' : n ++ [ '.' ]) (0 : t ++ [ 0 ])

part2 :: [String] -> Int
part2 xs = replace2 'S' '|' xs |> runBeam2 (replicate (length (head xs)) 0 |> setElement (elemIndex 'S' (head xs) |> fromJust) 1)

-- runStep (x1:'|':x2:xs) (y1:'^':y2:ys) = 
test = [ ".......S......."
       , "..............."
       , ".......^......."
       , "..............."
       , "......^.^......"
       , "..............."
       , ".....^.^.^....."
       , "..............."
       , "....^.^...^...."
       , "..............."
       , "...^.^...^.^..."
       , "..............."
       , "..^...^.....^.."
       , "..............."
       , ".^.^.^.^.^...^."
       , "..............."
       ]