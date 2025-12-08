import           Data.List
import qualified Data.Map  as Map
import           MUtils

type NodeId = Int
type CircuitId = Int

type Node = ((Int, Int, Int), NodeId)
type Connection = (NodeId, NodeId)
type State = (Map.Map CircuitId [NodeId], Map.Map NodeId CircuitId)

buildConnections :: [String] -> [Connection]
buildConnections xs = map (splitOn ',') xs |> map2 readInt |> map t3fromList |> zipWithIndexes |> combinationsSelf
    |> sortOn (uncurry distanceSquared) |> map (mapBoth snd)

distanceSquared :: Node -> Node -> Int
distanceSquared ((a, b, c), _) ((d, e, f), _) = (a - d) ^ 2 + (b - e) ^ 2 + (c - f) ^ 2

connectNodes :: Connection -> State -> State
connectNodes (x, y) (c, rc) = (c', rc')
  where
    [lowId, highId] = sort [ rc Map.! x, rc Map.! y ]
    movedNodes = c Map.! highId
    c' = c |> Map.adjust (++ movedNodes) lowId |> Map.insert highId []
    rc' = foldr (`Map.insert` lowId) rc movedNodes

buildCircuits :: Int -> [Connection] -> State -> State
buildCircuits 0 _ s = s
buildCircuits n ((x1, x2) : xs) s@(c, rc) = buildCircuits (n - 1) xs (if rc Map.! x1 == rc Map.! x2 then s else connectNodes (x1, x2) s)

startState :: Int -> State
startState n = [ 0 .. n - 1 ] |> zipF id |> Map.fromList |> replicate 2 |> t2fromList |> mapFst (Map.map singleton)

part1 :: [String] -> Int
part1 xs = buildCircuits 1000 (buildConnections xs) (startState (length xs)) |> fst |> Map.toList |> map snd |> map length |> sort |> reverse |> take 3 |> product

buildBigCircuit :: [Connection] -> State -> Connection
buildBigCircuit ((x1, x2) : xs) s@(c, rc)
    | rc Map.! x1 == rc Map.! x2 = buildBigCircuit xs s
    | length (c' Map.! 0) == 1000 = (x1, x2)
    | otherwise = buildBigCircuit xs (c', rc')
  where
    (c', rc') = connectNodes (x1, x2) s

getNodeX :: [String] -> NodeId -> Int
getNodeX xs n = xs !! n |> takeWhile (/= ',') |> readInt

part2 :: [String] -> Int
part2 xs = buildBigCircuit (buildConnections xs) (startState (length xs)) |> t2toList |> map (getNodeX xs) |> product

test = [ "162,817,812"
       , "57,618,57"
       , "906,360,560"
       , "592,479,940"
       , "352,342,300"
       , "466,668,158"
       , "542,29,236"
       , "431,825,988"
       , "739,650,466"
       , "52,470,668"
       , "216,146,977"
       , "819,987,18"
       , "117,168,530"
       , "805,96,715"
       , "346,949,466"
       , "970,615,88"
       , "941,993,340"
       , "862,61,35"
       , "984,92,344"
       , "425,690,689"
       ]