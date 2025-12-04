--Version 1.1

module GridUtils (gridFromList, gridFromNeighbourMap, getGridPosition, neighbourMapO, neighbourMapD, neighbourMap, buildNeighbours, directionsO, directionsD, directionsO3, directionsD3,
    getValue, changeValue, changeAllValues, updateNeighbour, getNeighbours, addPoints, mulPoint,
    pointDistanceO, getConnected, gridBounds, allPointsInRangeO, allNMPoints, showCharGrid, showStringGrid, showGrid, showGrid1,
    simplePathO, simplePathO',
    allDirs, rotateDirC, rotateDirA, flipDir, moveDir, stepDir, deflectDir, followDirPath, dirToPoint, pointToDirs, isHorizontal, isVertical,
    Grid, NeighbourMap, Point, Point3, Dir (East, West , North , South)) where
import MUtils
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List
import Data.Maybe (isJust)
import Data.Bifunctor ( Bifunctor(bimap) )

type Point = (Int, Int)
type Point3 = (Int, Int, Int)

type Grid a = Map.Map Point a
type NeighbourMap a = Map.Map Point (a, [(Point, a)])


gridFromList :: [[a]] -> Grid a
gridFromList ass = zipWithIndexes ass |> map (\(as,y) -> zipWithIndexes as |> map (\(a,x) -> Map.singleton (x,y) a)) |> concat |> Map.unions

gridFromNeighbourMap :: NeighbourMap a -> Grid a
gridFromNeighbourMap = Map.map fst

getGridPosition :: Eq a => Grid a -> a -> Maybe Point
getGridPosition g x = Map.foldrWithKey (\p a r-> if isJust r then r else if a==x then Just p else Nothing) Nothing g

neighbourMapO :: Grid a -> NeighbourMap a
neighbourMapO = neighbourMap directionsO

neighbourMapD :: Grid a -> NeighbourMap a
neighbourMapD = neighbourMap directionsD

neighbourMap :: [(Int,Int)] -> Grid a -> NeighbourMap a
neighbourMap directions grid = Map.toList grid |> map (\(point, a) -> (point, buildNeighbours directions grid point) ) |> Map.fromList

buildNeighbours :: [(Int,Int)] -> Grid a -> Point -> (a, [(Point, a)])  --TODO: combine membership check with lookup?
buildNeighbours directions grid point =  directions |> map (addPoints point) |> filter (`Map.member` grid) |> zipF (grid Map.!) |> pair (grid Map.! point)

hasNeighbour :: (a->Bool) -> Point -> NeighbourMap a -> Bool
hasNeighbour p (x,y) m = m Map.! (x,y) |> snd |> map snd |> filter p |> length |> (>0)

addPoints :: Num a => (a,a) -> (a,a) -> (a,a)
addPoints (x1,y1) (x2,y2) = (x1+x2, y1+y2)

mulPoint :: Num a => a -> (a,a) -> (a,a)
mulPoint n (x,y) = (x*n, y*n)

getValue :: NeighbourMap a -> Point -> a
getValue ngrid point = ngrid Map.! point |> fst

changeValue :: (a->a) -> Point -> NeighbourMap a -> NeighbourMap a
changeValue f point ngrid = foldr (\p newMap-> updateNeighbour newMap (fst p) point) (Map.adjust (mapFst f) point ngrid) (getNeighbours ngrid point)

updateNeighbour :: NeighbourMap a -> Point -> Point -> NeighbourMap a -- Updates the neighbour n of point s
updateNeighbour ngrid s n = ngrid Map.! s |> mapSnd (map (\(p,v)-> if p==n then (p, ngrid Map.! p |> fst) else (p,v))) |>  \newValue-> Map.insert s newValue ngrid

changeAllValues :: (a->b) -> NeighbourMap a -> NeighbourMap b
changeAllValues f = Map.map (bimap f (map (mapSnd f)))

getNeighbours :: NeighbourMap a -> Point -> [(Point, a)]
getNeighbours ngrid point = ngrid Map.! point |> snd

directionsO :: [(Int,Int)]
directionsO = [(0,-1), (-1,0), (1,0), (0,1)]

directionsD :: [(Int,Int)]
directionsD = [(-1,-1), (0,-1), (1,-1), (-1,0), (1,0), (-1,1), (0,1), (1,1)]

directionsO3 :: [(Int,Int,Int)]
directionsO3 = [(1,0,0), (-1,0,0), (0,1,0), (0,-1,0), (0,0,1), (0,0,-1)]

directionsD3 :: [(Int,Int,Int)]
directionsD3 = [(-1,-1,-1),(-1,-1,0),(-1,-1,1),(-1,0,-1),(-1,0,0),(-1,0,1),(-1,1,-1),(-1,1,0),(-1,1,1),(0,-1,-1),(0,-1,0),(0,-1,1),(0,0,-1),(0,0,1),(0,1,-1),(0,1,0),(0,1,1),(1,-1,-1),(1,-1,0),(1,-1,1),(1,0,-1),(1,0,0),(1,0,1),(1,1,-1),(1,1,0),(1,1,1)]

pointDistanceO :: Num a => (a,a) -> (a,a) -> a
pointDistanceO (x1,y1) (x2,y2) = abs (x1-x2) + abs (y1-y2)

gridBounds :: Grid a -> (Int,Int)
gridBounds g = (map fst g' |> map fst |> maximum, map fst g' |> map snd |> maximum) where g' = Map.toList g

allPointsInRangeO :: Grid a -> Int -> Point -> [Point]
allPointsInRangeO g r p = [-r..r] |> map (\dx-> let yMax = r - abs dx in [-yMax .. yMax] |> map (pair dx)) |> concat |> map (addPoints p) |> filter (`Map.member` g) |> filter (/=p)

allNMPoints :: NeighbourMap a -> [Point]
allNMPoints nm = combinations [0..x] [0..y] where (x,y) = gridFromNeighbourMap nm |> gridBounds

showCharGrid :: Grid Char -> [String]
showCharGrid g = [0..maxY] |> map (\y-> [0..maxX] |> map (\x-> g Map.! (x,y))) where
    (maxX, maxY) = gridBounds g

showStringGrid :: Grid String -> [String]
showStringGrid g = [0..maxY] |> map (\y-> [0..maxX] |> map (\x-> g Map.! (x,y))) |> map (joinWith "|") where
    (maxX, maxY) = gridBounds g

showGrid :: Show a => Grid a -> [String]
showGrid g = Map.map show g |> showStringGrid

showGrid1 :: Show a => Grid a -> [String]
showGrid1 g = Map.map (head . show) g |> showCharGrid


--Pathfinding

--Finds a simple orthogonal path between two points, and returns every value along the path, including the starting and ending nodes
simplePathO  :: Grid a -> Point -> Point -> [a]
simplePathO g p1 p2 = simplePathO' p1 p2 |> map (g Map.!)

simplePathO' :: Point -> Point -> [Point]
simplePathO' p1@(x1,y1) p2@(x2,y2)
    | x1==x2 && y1==y2 = [p1]
    | x1==x2 && y1>y2  = p1 : simplePathO' (x1, y1-1) p2
    | x1==x2 && y1<y2  = p1 : simplePathO' (x1, y1+1) p2
    | x1>x2            = p1 : simplePathO' (x1-1, y1) p2
    | x1<x2            = p1 : simplePathO' (x1+1, y1) p2

getConnected :: NeighbourMap a -> (a->Bool) -> Point -> [Point]
getConnected ngrid pr p = getConnected' ngrid pr [p] Set.empty |> Set.toList

getConnected' :: NeighbourMap a -> (a->Bool) -> [Point] -> Set.Set Point -> Set.Set Point
getConnected' g pr [] visited = visited
getConnected' g pr (next:frontier) visited = getConnected' g pr (uniqueSet (frontier++newNodes)) (Set.insert next visited) where
    newNodes = getNeighbours g next |> filter (pr . snd) |> map fst |> filter (`Set.notMember` visited)

---------- Dir stuff ----------

data Dir = East | West | North | South
    deriving ( Show, Read, Eq, Ord )

allDirs :: [Dir]
allDirs = [East, West, North, South]

--Rotates a direction anticlockwise
rotateDirA :: Dir -> Dir
rotateDirA North = West
rotateDirA South = East
rotateDirA East = North
rotateDirA West = South

--Rotates a direction clockwise
rotateDirC :: Dir -> Dir
rotateDirC North = East
rotateDirC South = West
rotateDirC West = North
rotateDirC East = South

--Flips a directions
flipDir :: Dir -> Dir
flipDir North = South
flipDir South = North
flipDir West = East
flipDir East = West

--Checks if a direction is equal to East or West
isHorizontal :: Dir -> Bool
isHorizontal dir = dir `elem` [ East, West ]

--Checks if a direction is equal to North or South
isVertical :: Dir -> Bool
isVertical dir = dir `elem` [ North, South ]

--Deflects a direction; if the given direction belong to the specified pair, the other direction from the pair is returned
deflectDir :: (Dir, Dir) -> Dir -> Maybe Dir
deflectDir (a, b) c
    | a == c = Just b
    | b == c = Just a
    | otherwise = Nothing

--Move a point in a given direction by a specified distance
moveDir :: Integral a => Dir -> a -> (a,a) -> (a,a)
moveDir North n (x, y) = (x, y - n)
moveDir South n (x, y) = (x, y + n)
moveDir West n (x, y) = (x - n, y)
moveDir East n (x, y) = (x + n, y)

--Moves a point one step in a given direction
stepDir :: Dir -> Point -> Point
stepDir dir = moveDir dir 1

--Moves a point by every direction in a list
followDirPath :: [Dir] -> Point -> Point
followDirPath dirs point = foldr stepDir point dirs

--Converts a dir to a point indicating that direction
dirToPoint :: Dir -> Point
dirToPoint North = (0, -1)
dirToPoint South = (0, 1)
dirToPoint East = (-1, 0)
dirToPoint West = (1, 0)

--Converts a point to a list of directions corresponding to that displacement
pointToDirs :: Point -> [Dir]
pointToDirs (0, 0) = []
pointToDirs (x, 0)
    | x > 0 = West : pointToDirs (x - 1, 0)
    | x < 0 = East : pointToDirs (x + 1, 0)
pointToDirs (x, y)
    | y > 0 = North : pointToDirs (x, y - 1)
    | y < 0 = South : pointToDirs (x, y + 1)






