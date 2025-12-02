-- Version 1.0.8
module MUtils (
   runOnFile, runTestOnFile, runOnFileGroup,
   putList,putListToStr, split, splitOn, count, count2, countEqual, maxOn, minOn, uniqueSet, unique, unique', uniqueOn, indexesWhere, replace, replace2, replace3, replaceIf, replaceIf2, replaceIf3, combinations, combinations3, combinationsSelf,
   (!!?), joinWith, zipWithIndexes, differences, indexes, zipF, binarySearch, histogram,
   indexes2, zipWithIndexes2, empty2, empty3, setElement, setElement2, setElement3, changeElement, changeElement2, changeElement3, removeElement, removeElement2, removeElement3, directions2D, directions3D, groupInto2D,
   map2, map3, filter2, filter3, zip2d, zip3d, findIndex2,
   pair, pairS, tupleWith, mapFst, mapSnd, mapBoth, fst3, snd3, thd3, fst4, snd4, thd4, frh4, t2toList, t3toList, t4toList, t5toList, t2fromList, t3fromList, t4fromList, t5fromList,
   flattenMaybe, removeNothing,
   repeatF, repeatUntil, examine, examineStr, examineRepeat,
   factorial, (//), nck, valueBetween, mean, meanI, sign,
   aStar, tryAStar, bfsAllCosts, memoizedCount, (|>), readInt
    ) where

import Control.Monad
import Data.List
import Data.Maybe
import System.IO
import qualified Data.Map as Map
import qualified Data.Set as Set

------------------------------------ File Utils ------------------------------------

--Takes a file path and a function, runs that function on the file's contents, and prints the function's output. Trims the last line of the file if it's an empty line
runOnFile :: Show a => String -> ([String]->a) -> IO ()
runOnFile input start = do
   handle <- openFile input ReadMode
   contents <- hGetContents handle
   let lines = splitOn '\n' contents
   let linesTrimmed = if last lines == "" then init lines else lines
   print $ start linesTrimmed
   hClose handle

runTestOnFile :: String -> ([String]->IO()) -> IO ()
runTestOnFile input start = do
   handle <- openFile input ReadMode
   contents <- hGetContents handle
   let lines = splitOn '\n' contents
   let linesTrimmed = if last lines == "" then init lines else lines
   start linesTrimmed
   hClose handle

--Same as run on file, but splits the resulting array of strings by empty lines
runOnFileGroup :: Show a => String -> ([[String]]->a) -> IO ()
runOnFileGroup input start = do
   handle <- openFile input ReadMode
   contents <- hGetContents handle
   let lines = splitOn '\n' contents
   let linesTrimmed = if last lines == "" then init lines else lines
   let res = splitOn "" linesTrimmed
   print $ start res
   hClose handle

------------------------------------ 1D List Utils ------------------------------------

----- Printing -----
putList :: [String] -> IO()
putList xs = joinWith "\n" xs |> putStrLn

putListToStr :: Show a => [a] -> IO()
putListToStr xs = map show xs |> joinWith "\n" |> putStrLn

----- Splitting -----
split     :: (a -> Bool) -> [a] -> [[a]]
split p s =  case dropWhile p s of
                      [] -> []
                      s' -> w : split p s''
                            where (w, s'') = break p s'

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn a xs = splitOn' a xs []

splitOn' :: Eq a => a -> [a] -> [a]-> [[a]]
splitOn' a [] op     = [reverse op]
splitOn' a (x:xs) op = if a==x then (reverse op):(splitOn' a xs []) else splitOn' a xs (x:op)

----- Checking properties -----
count :: (a->Bool) -> [a] -> Int
count p = length . filter p

count2 :: (a->Bool) -> [[a]] -> Int
count2 p as = map (count p) as |> sum

countEqual :: Eq a => a -> [a] -> Int
countEqual x = count (==x)

countWhile :: (a->Bool) -> [a] -> Int
countWhile p [] = 0
countWhile p (x:xs)
   | p x       = 1 + countWhile p xs
   | otherwise = 0

----- Finding elements -----
maxOn :: Ord b => (a->b) -> [a] -> a
maxOn f [x] = x
maxOn f (x:xs) = if f x >= f best then x else best where best = maxOn f xs

minOn :: Ord b => (a->b) -> [a] -> a
minOn f [x] = x
minOn f (x:xs) = if f x <= f best then x else best where best = maxOn f xs

----- Filtering -----
uniqueSet :: Ord a  => [a] -> [a]
uniqueSet xs = Set.fromList xs |> Set.toList

--Removes duplicates from a list, preserving order
unique :: Eq a  => [a] -> [a]
unique xs = xs |> reverse |> unique' |> reverse

--Removes duplicates from a list faster, but messes up the order
unique' :: Eq a => [a] -> [a]
unique' []     = []
unique' (x:xs) = if x `elem` xs then unique' xs else x:unique' xs

uniqueOn :: Eq b => (a->b) -> [a] -> [a]
uniqueOn f as = as |> reverse |> uniqueOn' f |> reverse

uniqueOn' :: Eq b => (a->b) -> [a] -> [a]
uniqueOn' f as = uniqueOn'' as (map f as)

uniqueOn'' :: Eq b => [a] -> [b] -> [a]
uniqueOn'' [] _ = []
uniqueOn'' (a:as) (b:bs) = if b `elem` bs then uniqueOn'' as bs else a:uniqueOn'' as bs

indexesWhere :: (a->Bool) -> [a] -> [Int]
indexesWhere p xs = zipWithIndexes xs |> filter (p . fst) |> map snd

----- Replacing -----

replace :: Eq a => a -> a -> [a] -> [a]
replace a b = map (\x-> if x==a then b else x)

replace2 :: Eq a => a -> a -> [[a]] -> [[a]]
replace2 a b = map2 (\x-> if x==a then b else x)

replace3 :: Eq a => a -> a -> [[[a]]] -> [[[a]]]
replace3 a b = map3 (\x-> if x==a then b else x)

replaceIf :: (a->Bool) -> a -> [a] -> [a]
replaceIf p a = map (\x-> if p x then a else x)

replaceIf2 :: (a->Bool) -> a -> [[a]] -> [[a]]
replaceIf2 p a = map2 (\x-> if p x then a else x)

replaceIf3 :: (a->Bool) -> a -> [[[a]]] -> [[[a]]]
replaceIf3 p a = map3 (\x-> if p x then a else x)

----- Combinations -----

combinations :: [a] -> [b] -> [(a,b)]
combinations [] _ = []
combinations (a:as) bs = map (a,) bs ++ combinations as bs

combinations3 :: [a] -> [b] -> [c] -> [(a,b,c)]
combinations3 as bs cs = map (\a-> map (\b-> map (a,b,) cs) bs) as |> concat |> concat

--Produces all combinations of two elements from an array, without symmetric of reflective ones. [x,y,z] produces [(x,y),(x,z),(y,z)], but not (y,x) or (x,x)
combinationsSelf :: [a] -> [(a,a)]
combinationsSelf [] = []
combinationsSelf (x:xs) = map (x,) xs ++ combinationsSelf xs

----- Other -----

--Equivalent of !!, but returns Nothing if there is no such element in a list and Just a otherwise
(!!?) :: [a] -> Int -> Maybe a
list !!? index = if index<0 || index>=length list then Nothing else Just (list!!index)

joinWith :: [a] -> [[a]] -> [a]
joinWith a [] = []
joinWith a [x] = x
joinWith a (x:xs) = (x++a)++(joinWith a xs)

indexes :: [a] -> [Int]
indexes [] = []
indexes a = [0..(length a)-1]

zipWithIndexes :: [a] -> [(a,Int)]
zipWithIndexes a = zip a (indexes a)

--Turns a list of numeric values into a list of differences between neighbouring values
differences :: Num a => [a] -> [a]
differences [] = []
differences a = zip (tail a) (init a) |>  map (uncurry (-))

zipF :: (a->b) -> [a] -> [(a,b)]
zipF f xs = zip xs (map f xs)

--For a given range of integers, which can be divided into two ranges, such that the second one matches a predicate, returns the start of that second range
binarySearch :: Integral i => (i->Bool) -> (i,i) -> i
binarySearch p (x,y) | x==y = x
   | p (x + (y-x) `div` 2) = binarySearch p (x, x + (y-x) `div` 2)
   | otherwise =  binarySearch p (x + (y-x) `div` 2 + 1, y)
   
--Produces a list of tuples, showing how common is each element in the input list. Not very efficient.
histogram :: Ord a => [a] -> [(Int, a)]
histogram xs = zip (map (\x-> count (==x) xs) xs') xs' |> sortOn fst where xs' = sort xs |> unique

------------------------------------ Higher-dimension List Utils ------------------------------------

indexes2 :: [[a]] -> [(Int, Int)]
indexes2 [] = []
indexes2 xs = combinations (indexes (head xs)) (indexes xs)

zipWithIndexes2 :: [[a]] -> [[(a,(Int,Int))]]
zipWithIndexes2 a =  map zipWithIndexes a |> zipWithIndexes |> map (\(as, y)-> map (\(p, x)-> (p,(x,y))) as)

empty2 :: Eq a => [[a]] -> Bool
empty2 xs = not $ any (/=[]) xs

empty3 :: Eq a => [[[a]]] -> Bool
empty3 xss = map (not . any (/=[])) xss |> and

setElement :: Int -> a -> [a] -> [a]
setElement i x xs = (take i xs)++[x]++(drop (i+1) xs)

setElement2 :: Int -> Int -> a -> [[a]] -> [[a]]
setElement2 i j x xs = (take j xs)++[setElement i x (xs!!j)]++(drop (j+1) xs)

setElement3 :: Int -> Int -> Int -> a -> [[[a]]] -> [[[a]]]
setElement3 i j k x xs = (take k xs)++[setElement2 i j x (xs!!k)]++(drop (k+1) xs)

changeElement :: Int -> (a->a) -> [a] -> [a]
changeElement i f [] = []
changeElement 0 f (x:xs) = (f x):xs
changeElement i f (x:xs) = x:(changeElement (i-1) f xs)

changeElement2 :: Int -> Int -> (a->a) -> [[a]] -> [[a]]
changeElement2 i j f xs = (take j xs)++[changeElement i f (xs!!j)]++(drop (j+1) xs)

changeElement3 :: Int -> Int -> Int -> (a->a) -> [[[a]]] -> [[[a]]]
changeElement3 i j k f xs = (take k xs)++[changeElement2 i j f (xs!!k)]++(drop (k+1) xs)

removeElement :: Int -> [a] -> [a]
removeElement i xs = take i xs ++ drop (i+1) xs

removeElement2 :: Int -> Int -> [[a]] -> [[a]]
removeElement2 i j xs = take j xs ++[removeElement i (xs!!j)] ++drop (j+1) xs

removeElement3 :: Int -> Int -> Int -> [[[a]]] -> [[[a]]]
removeElement3 i j k xs = take k xs ++[removeElement2 i j (xs!!k)] ++drop (k+1) xs

directions2D :: [(Int,Int)]
directions2D = [(-1,-1), (0,-1), (1,-1), (-1,0), (1,0), (-1,1), (0,1), (1,1)]

directions3D :: [(Int,Int,Int)]
directions3D = [(-1,-1,-1),(-1,-1,0),(-1,-1,1),(-1,0,-1),(-1,0,0),(-1,0,1),(-1,1,-1),(-1,1,0),(-1,1,1),(0,-1,-1),(0,-1,0),(0,-1,1),(0,0,-1),(0,0,1),(0,1,-1),(0,1,0),(0,1,1),(1,-1,-1),(1,-1,0),(1,-1,1),(1,0,-1),(1,0,0),(1,0,1),(1,1,-1),(1,1,0),(1,1,1)]

--Groups elements of a list into groups of a specified size
groupInto2D :: Int -> [a] -> [[a]]
groupInto2D _ [] = []
groupInto2D n as = (take n as):(groupInto2D n (drop n as))

---- Higher-dimension versions of prelude functions ----
map2 :: (a->b) -> [[a]] -> [[b]]
map2 f = map (map f)

map3 :: (a->b) -> [[[a]]] -> [[[b]]]
map3 f = map (map (map f))

filter2 :: (a->Bool) -> [[a]] -> [[a]]
filter2 p = map (filter p)

filter3 :: (a->Bool) -> [[[a]]] -> [[[a]]]
filter3 p = map2 (filter p)

--zip3 exists and combines 3 lists, so using 2d and 3d here
zip2d :: [[a]] -> [[b]] -> [[(a,b)]]
zip2d [] _ = []
zip2d _ [] = []
zip2d (a:as) (b:bs) = (zip a b):(zip2d as bs)

zip3d :: [[[a]]] -> [[[b]]] -> [[[(a,b)]]]
zip3d [] _ = []
zip3d _ [] = []
zip3d (a:as) (b:bs) = (zip2d a b):(zip3d as bs)

findIndex2 :: (a->Bool) ->[[a]] -> (Int, Int)
findIndex2 p []           = error "Element not found"
findIndex2 p (xs:xss) = case findIndex p xs of
   Nothing -> mapSnd (+1) (findIndex2 p xss)
   (Just i)-> (i,0)

------------------------------------ Tuple Utils ------------------------------------

pair :: a -> b -> (a, b)
pair a b = (a,b)
pairS :: a -> b -> (b, a)
pairS a b = (b, a)

tupleWith :: (a->b) -> a -> (a,b)
tupleWith f a = (a, f a)

mapFst :: (a->c) -> (a,b) -> (c, b)
mapFst f (a,b) = (f a, b)
mapSnd :: (b->c) -> (a,b) -> (a,c)
mapSnd f (a,b) = (a, f b)
mapBoth :: (a -> b) -> (a, a) -> (b, b)
mapBoth f (x,y) = (f x, f y)

fst3 :: (a,b,c) -> a
fst3 (a,b,c) = a
snd3 :: (a,b,c) -> b
snd3 (a,b,c) = b
thd3 :: (a,b,c) -> c
thd3 (a,b,c) = c

fst4 :: (a,b,c,d) -> a
fst4 (a,b,c,d) = a
snd4 :: (a,b,c,d) -> b
snd4 (a,b,c,d) = b
thd4 :: (a,b,c,d) -> c
thd4 (a,b,c,d) = c
frh4 :: (a,b,c,d) -> d
frh4 (a,b,c,d) = d

t2toList :: (a,a) -> [a]
t2toList (x,y) = [x,y]
t3toList :: (a,a,a) -> [a]
t3toList (x,y,z) = [x,y,z]
t4toList :: (a,a,a,a) -> [a]
t4toList (x1,x2,x3,x4) = [x1,x2,x3,x4]
t5toList :: (a,a,a,a,a) -> [a]
t5toList (x1,x2,x3,x4,x5) = [x1,x2,x3,x4,x5]

t2fromList :: [a] -> (a,a)
t2fromList [x,y] = (x,y)
t3fromList :: [a] -> (a,a,a)
t3fromList [x,y,z] = (x,y,z)
t4fromList :: [a] -> (a,a,a,a)
t4fromList [x1,x2,x3,x4] = (x1,x2,x3,x4)
t5fromList :: [a] -> (a,a,a,a,a)
t5fromList [x1,x2,x3,x4,x5] = (x1,x2,x3,x4,x5)

------------------------------------ Maybe Utils ------------------------------------

flattenMaybe :: Maybe (Maybe a) -> Maybe a
flattenMaybe Nothing = Nothing
flattenMaybe (Just Nothing) = Nothing
flattenMaybe (Just (Just a)) = Just a

removeNothing :: [Maybe a] -> [a]
removeNothing [] = []
removeNothing (Nothing:xs) = removeNothing xs
removeNothing ((Just a):xs) = a:(removeNothing xs)

------------------------------------ Function Utils ------------------------------------

repeatF :: Int -> (a->a) -> a -> a
repeatF 0 _ x = x
repeatF n f x = repeatF (n-1) f (f x)

repeatUntil :: (a->Bool) -> (a->a) -> a -> a
repeatUntil p f a = if p a then a else repeatUntil p f (f a)


--For testing: shows the function's output for a list of inputs
examine :: Show a => Show b => (a->b) -> [a] -> IO()
examine f xs = map (\x-> (x, f x)) xs |> map (\(a,b)-> (show a)++(": ")++(show b)) |> joinWith "\n" |> putStrLn

--Similar to examine, for when the result of the examined function is a (potentially) multi-line string
examineStr :: Show a => (a->String) -> [a] -> IO()
examineStr f xs = map (\x-> (x, f x)) xs |> map (\(a,b)-> (show a)++(":\n")++b) |> joinWith "\n" |> putStrLn

--For testing: shows the results of a function's repeated applications
examineRepeat :: Show a => (a->a) -> a -> Int -> IO()
examineRepeat f a n = examine (\x-> repeatF x f a) [0..n]

------------------------------------ Math ------------------------------------

factorial :: (Integral a) => a -> a
factorial x = product [1..x]

(//) :: (Integral a, Integral b) => a -> b -> Float
x // y = (fromIntegral x) / (fromIntegral y)

nck :: (Integral a) => a -> a -> a
nck n k = (factorial n) `div` ((factorial k) * (factorial (n-k)))

valueBetween :: Ord a => (a,a) -> a -> Bool
valueBetween (low,high) x = x >= low && x <= high

mean :: Fractional a => [a] -> a
mean as = (sum as) / (length as |> fromIntegral)

meanI ::  Integral a => [a] -> Float
meanI as = (sum as |> fromIntegral) / (length as |> fromIntegral)

sign :: Int -> Int
sign x
      | x > 0  = 1
      | x == 0 = 0
      | x < 0  = -1

------------------------------------ A* ------------------------------------

--               Neighbours         Heuristic  Start  isTarget    Cost of shortest path
aStar :: Ord a => (a->[(a, Int)]) -> (a->Int) -> a -> (a->Bool) -> Int
aStar neighbours heuristic start target = fromMaybe (error "aStar: No path found") res
   where res = aStar' neighbours heuristic [(start, 0, heuristic start)] Set.empty target

tryAStar :: Ord a => (a->[(a, Int)]) -> (a->Int) -> a -> (a->Bool) -> Maybe Int
tryAStar neighbours heuristic start target = aStar' neighbours heuristic [(start, 0, heuristic start)] Set.empty target

--                neighbours        heuristic    frontier           visited      isTarget
aStar' :: Ord a => (a->[(a, Int)]) -> (a->Int) -> [(a, Int, Int)] -> Set.Set a -> (a->Bool) -> Maybe Int
aStar' _  _ []              _  _ = Nothing --The frontier being empty means we've explored everything
aStar' ns h (next:frontier) vs t
   | t (fst3 next)             = Just (snd3 next)
   | Set.member (fst3 next) vs = aStar' ns h frontier vs t
   | otherwise                 = aStar' ns h (expandFrontier frontier newNodes) (Set.insert (fst3 next) vs) t where
      newNodes = ns (fst3 next) |> filter (\(a,_)-> a `Set.notMember` vs) |> map (\(a,c) -> (a, c + snd3 next, h a))

expandFrontier :: [(a, Int, Int)] -> [(a, Int, Int)] -> [(a, Int, Int)]
expandFrontier frontier newNodes = foldl insertNode frontier newNodes

insertNode :: [(a, Int, Int)] -> (a, Int, Int) -> [(a, Int, Int)]
insertNode [] node = [node]
insertNode ((a1,c1,h1):xs) (a2,c2,h2) = if c2+h2 < c1+h1 then (a2,c2,h2):(a1,c1,h1):xs else (a1,c1,h1):(insertNode xs (a2,c2,h2))

--Does bfs and returns a map with the cost of getting to each state. Useful for setting up heuristics for later aStar traversal with more restrictions
bfsAllCosts :: Ord a => (a->[(a, Int)]) -> a -> Map.Map a Int
bfsAllCosts neighbours start = bfsAllCosts' neighbours [(start,0)] Set.empty Map.empty

bfsAllCosts' :: Ord a => (a->[(a, Int)]) ->  [(a, Int)] -> Set.Set a -> Map.Map a Int -> Map.Map a Int
bfsAllCosts' n [] _ m = m
bfsAllCosts' n (next:frontier) visited m
   | Set.member (fst next) visited = bfsAllCosts' n frontier visited m
   | otherwise = bfsAllCosts' n newFrontier (Set.insert (fst next) visited) (uncurry Map.insert next m)  where
      newFrontier = n (fst next) |> map (mapSnd (+snd next)) |> filter ((`Set.notMember` visited) . fst) |> foldl insertNodeBfs frontier

insertNodeBfs :: [(a, Int)] -> (a, Int) -> [(a, Int)]
insertNodeBfs [] node = [node]
insertNodeBfs ((a1,c1):xs) (a2,c2)
   | c2 < c1 = (a2,c2):(a1,c1):xs
   | otherwise =  (a1,c1):insertNodeBfs xs (a2,c2)

------------------------------------ Memoized counter ------------------------------------

-- Help with counting how many ways something can be done in a memoized way. Takes a function that for a given state returns either
-- a list of states that need to be summed or an integer indicating the number of possibilities from this state.
memoizedCount :: Ord a => (a-> Either [a] Integer) -> a -> Integer
memoizedCount f s = memoizedCount' f s Map.empty |> (Map.! s)

memoizedCount' :: Ord a => (a-> Either [a] Integer) -> a -> Map.Map a Integer -> Map.Map a Integer
memoizedCount' f s m | Map.member s m = m
memoizedCount' f s m = case f s of
   Left as -> case foldl (mcProcessOptions f) (m, 0) as of
      (newMap, n) -> Map.insert s n newMap
   Right n -> Map.insert s n m

mcProcessOptions :: Ord a => (a-> Either [a] Integer) -> (Map.Map a Integer, Integer) -> a -> (Map.Map a Integer, Integer)
mcProcessOptions f (m, n) s = (newMap, n + newMap Map.! s) where newMap = memoizedCount' f s m

------------------------------------ Misc ------------------------------------

(|>) :: a -> (a->b) -> b
a |> f = f a

readInt :: String -> Int
readInt = read



-- Notes about updating from MyUtils:
-- Replace exists with any
-- Replace rotateMatrix with transpose
-- Replace "tupleMap f" with "map (uncurry f)"
-- Replace "freq xs x" with "countEqual x xs"
-- Replace "separate" with "swap . break"