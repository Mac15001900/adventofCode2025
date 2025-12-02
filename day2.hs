import           MUtils

--A nice and really fast part 1
dSize :: Int -> Int
dSize n = show n |> length

isSilly :: Int -> Bool
isSilly n = even s && (n `mod` sillyBase s == 0)
  where
    s = dSize n

sillyBase :: Int -> Int
sillyBase size = 10 ^ (size `div` 2) + 1

smallestSilly :: Int -> Int
smallestSilly size = sillyBase size |> (* 10 ^ ((size `div` 2) - 1))

biggestSilly :: Int -> Int
biggestSilly size = sillyBase size |> (* ((10 ^ (size `div` 2)) - 1))

nextSilly :: Int -> Int
nextSilly n
    | isSilly n = if isSilly res then res else sillyBase (2 + size)
    | size `mod` 2 == 1 = sillyBase (size + 1)
    | otherwise = if res2 >= n then res2 else nextSilly res2
  where
    res = n + sillyBase (dSize n)
    size = dSize n
    res2 = sillify n

sillify :: Int -> Int
sillify n = show n |> \x -> take (length x `div` 2) x |> read |> (* sillyBase (dSize n))

countSilly :: Int -> Int -> Int
countSilly l u
    | l'' > u = 0
    | ls'' == us = sumSilliness l'' u
    | otherwise = countSilly l'' (biggestSilly ls'') + countSilly (smallestSilly (ls'' + 2)) u
  where
    ls = dSize l
    us = dSize u
    l' = if even ls then sillify l else smallestSilly (ls + 1)
    l'' = if l' >= l then l' else nextSilly l'
    ls'' = dSize l''

sumSilliness :: Int -> Int -> Int
sumSilliness l u = l * amount + ((amount * (amount - 1)) `div` 2) * base
  where
    base = sillyBase (dSize l)
    amount = 1 + ((u - l) `div` base)

parse :: String -> [(Int, Int)]
parse xs = splitOn ',' xs |> map (splitOn '-') |> map2 readInt |> map t2fromList

part1 :: [String] -> Int
part1 xs = parse (head xs) |> map (uncurry countSilly) |> sum

--A brute force part 2 xD
isSillier :: Int -> Bool
isSillier xs = [ 1 .. (length (show xs) `div` 2) ] |> map (\n -> groupInto2D n (show xs)) |> map unique |> map length |> elem 1

part2 :: [String] -> Int
part2 xs = parse (head xs) |> map (\(a, b) -> [ a .. b ]) |> concat |> filter isSillier |> sum

test = [ "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124" ]