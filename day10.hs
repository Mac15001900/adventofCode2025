{-# LANGUAGE TupleSections #-}

import           Data.List
import qualified Data.Map   as Map
import           Data.Maybe

import           MUtils

type Lamps = Map.Map Int Bool

type LampTarget = [Bool]

type JoltageTarget = [Int]

type Button = [Int]

type Machine = (LampTarget, [Button], JoltageTarget) --Target lights, buttons, mystery thing

parse :: String -> Machine
parse xs = (target, buttons, joltage)
  where
    target = tail xs |> takeWhile (/= ']') |> map (== '#')
    buttons = dropWhile (/= ']') xs |> tail |> takeWhile (/= '{') |> filter (`notElem` "()") |> splitOn ' ' |> filter (not . null)
        |> map (splitOn ',') |> map2 readInt
    joltage = dropWhile (/= '{') xs |> tail |> init |> splitOn ',' |> map readInt

pressButton :: Lamps -> Button -> Lamps
pressButton l bs = foldr (Map.adjust not) l bs

findCost :: Machine -> Int
findCost m@(t, bs, _) = aStar (\l' -> map (pressButton l') bs |> map (, largestButton m)) (distance t) (map (, False) (indexes t) |> Map.fromList) ((== 0) . distance t)
    |> (`div` largestButton m)

largestButton :: Machine -> Int
largestButton (_, bs, _) = map length bs |> maximum

distance :: LampTarget -> Lamps -> Int
distance t ls = zipWithIndexes t |> count (\(t, i) -> ls Map.! i /= t)

part1 :: [String] -> Int
part1 xs = map parse xs |> map findCost |> sum

test = [ "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}"
       , "[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}"
       , "[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}"
       ]

---------------------------------- Part 2 ------------------------
type Range = (Int, Int)

type VariableId = Int

type Assignments = [Range] --nth element is the range for variable n

type Equation = ([VariableId], Int) --Each element is a list of variables that sum to a given value

type Problem = ([Equation], [Button], JoltageTarget)

type Refiner =
    (Problem -> Assignments -> Maybe Assignments) -- A function that performs some deductions and possibly narrows down options (or finds a contradition and returns Nothing)

buildProblem :: Machine -> Problem
buildProblem (_, bs, jt) = (indexes jt |> map (\i -> (indexes bs |> filter (\i' -> i `elem` (bs !! i')), jt !! i)) |> expandEquations, bs, jt)

expandEquations :: [Equation] -> [Equation]
expandEquations eqs = if null newEqs then eqs else expandEquations (eqs ++ newEqs)
  where
    newEqs = eqs |> map (\eq -> filter (\eq' -> length (fst eq') > length (fst eq)) eqs |> filter (\eq' -> length (intersect (fst eq) (fst eq')) == length (fst eq))
                         |> map (\(vs, s) -> (filter (`notElem` (fst eq)) vs, s - snd eq))) |> concat |> unique |> filter (`notElem` eqs)

basicAssignments :: Problem -> Assignments
basicAssignments (eqs, bs, _) = indexes bs |> map (\b -> filter (elem b . fst) eqs |> map snd |> minimum |> (0, ))

--Update the range for a given variable based on a given equation. We assume that variable is present in the equation.
makeRange :: Assignments -> VariableId -> Equation -> Maybe Range
makeRange as n (vs, s) = if isValid r && snd r <= s then Just r else Nothing
  where
    minValue = filter (/= n) vs |> map (as !!) |> map fst |> sum
    maxValue = filter (/= n) vs |> map (as !!) |> map snd |> sum |> min s
    r = (s - maxValue, s - minValue)

combineRanges :: [Maybe Range] -> Maybe Range
combineRanges [] = Nothing
combineRanges rs = if any isNothing rs then Nothing else catMaybes rs |> foldr1 (\(l1, u1) (l2, u2) -> (max l1 l2, min u1 u2)) |> \r -> if isValid r then Just r else Nothing

isValid :: Range -> Bool
isValid (l, u) = l <= u && l >= 0

basicUpdates :: Problem -> Assignments -> Maybe Assignments
basicUpdates (eqs, bs, _) as = if any isNothing as' then Nothing else Just (catMaybes as')
  where
    updateVariable n = map (makeRange as n) (filter (\e -> elem n (fst e)) eqs) |> (Just (as !! n) :) |> combineRanges
    as' = foldr (\n as'' -> setElement n (updateVariable n) as'') (map Just as) (indexes bs)

applyCap :: Int -> Problem -> Assignments -> Maybe Assignments
applyCap n _ as = if dof >= 0 && all isValid as' then Just as' else Nothing
  where
    dof = n - sum (map fst as)
    as' = map (\(l, u) -> (l, min u (l + dof))) as

applyRefinements :: Problem -> [Refiner] -> Assignments -> Maybe Assignments
applyRefinements p rs as
    | isNothing as' = Nothing
    | as == fromJust as' = Just as
    | otherwise = applyRefinements p rs (fromJust as')
  where
    as' = foldr (\r as'' -> r p =<< as'') (Just as) rs

isDone :: Maybe Assignments -> Bool
isDone = maybe False (all (\(l, u) -> l == u))

solve :: Machine -> Int
solve m = let p = buildProblem m in solve' p (sum (thd3 p)) (basicAssignments p) |> fromJust

solve' :: Problem -> Int -> Assignments -> Maybe Int
solve' p bestSol as
    | isNothing as' = Nothing
    | isDone as' = Just (score (fromJust as'))
    | otherwise = catMaybes [ solL, solU ] |> sort |> map Just |> (++ [ Nothing ]) |> head
  where
    as' = applyRefinements p [ basicUpdates, applyCap bestSol ] as
    [asL, asU] = bisectLargest (fromJust as')
    solU = solve' p bestSol asU
    bestSol' = if isJust solU then min (fromJust solU) bestSol else bestSol
    solL = solve' p bestSol' asL

bisectLargest :: Assignments -> [Assignments]
bisectLargest as = [ (l, (l + u) `div` 2), ((l + u) `div` 2 + 1, u) ] |> map (\a -> setElement maxI a as)
  where
    maxI = zipWithIndexes as |> maxOn ((\(a, b) -> b - a) . fst) |> snd
    (l, u) = as !! maxI

score :: Assignments -> Int
score as = map fst as |> sum

part2 :: [String] -> Int
part2 xs = map parse xs |> map solve |> sum

main :: IO ()
main = do
    putStrLn "Starting..."
    runOnFile "input10.txt" part2
    putStrLn "Done!"
