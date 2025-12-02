import           MUtils

part1 :: [String] -> Int
part1 xs = countZeros xs 50

countZeros :: [String] -> Int -> Int
countZeros [] 0 = 1
countZeros [] _ = 0
countZeros (x : xs) n = (if n == 0 then 1 else 0) + countZeros xs ((n + (parseInstruction x)) `mod` 100)

parseInstruction :: String -> Int
parseInstruction ('L' : xs) = -(read xs)
parseInstruction ('R' : xs) = read xs

part2 :: [String] -> Int
part2 xs = countZeros2 xs 50

countZeros2 :: [String] -> Int -> Int
countZeros2 [] _ = 0
countZeros2 (x : xs) n = (rotationClicks n dn) + (countZeros2 xs ((n + dn) `mod` 100))
  where
    dn = parseInstruction x

rotationClicks :: Int -> Int -> Int
rotationClicks n dn = ((abs n') `div` 100) + (if sign n == -(sign n') then 1 else 0) + (if n' == 0 then 1 else 0)
  where
    n' = n + dn

test = [ "L68", "L30", "R48", "L5", "R60", "L55", "L1", "L99", "R14", "L82" ]