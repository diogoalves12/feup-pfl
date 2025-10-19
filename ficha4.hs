import Data.Char

-- EX1
calcPi1 :: Int -> Double
calcPi1 n = sum (take n terms)
  where
    signs = cycle [1, -1]
    denominators = [1,3..n]
    terms = zipWith (\s d -> fromIntegral s * 4 / fromIntegral d) signs denominators

calcPi2 :: Int -> Double
calcPi2 n = 3 + sum (take n terms)
  where
    signs = cycle [1, -1]
    denominators = [2,4..n]
    terms = zipWith term signs denominators
    term s d = fromIntegral s * 4 / (fromIntegral d * (fromIntegral d + 1) * (fromIntegral d + 2))


-- EX2

-- Example 4 (cont.) – Infinite lists of prime numbers
primes :: [Integer]
primes = sieve [2..]

sieve :: [Integer] -> [Integer]
sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

twinPrimes :: [(Integer, Integer)]
twinPrimes = [(p, q) | (p, q) <- zip primes (tail primes), q - p == 2]

-- EX3 & EX4
hammings :: [Integer]
hammings = 1 : merge3 (map (2*) hammings)
                       (map (3*) hammings)
                       (map (5*) hammings)

merge :: [Integer] -> [Integer] -> [Integer]
merge (x:xs) (y:ys)
  | x < y     = x : merge xs (y:ys)
  | x > y     = y : merge (x:xs) ys
  | otherwise = x : merge xs ys

merge3 :: [Integer] -> [Integer] -> [Integer] -> [Integer]
merge3 a b c = merge a (merge b c)


{-
------------------------------------------------------------------------------------------
------------------------------------ REVER(All BELOW) ------------------------------------
------------------------------------------------------------------------------------------
-}

-- EX5 
-- a)
rot13Char :: Char -> Char
rot13Char c
  | isAsciiLower c = chr (((ord c - ord 'a' + 13) `mod` 26) + ord 'a')
  | isAsciiUpper c = chr (((ord c - ord 'A' + 13) `mod` 26) + ord 'A')
  | otherwise      = c

rot13 :: String -> String
rot13 = map rot13Char

main_rot13 :: IO ()
main_rot13 = do
  putStrLn "Enter text to apply ROT13:"
  txt <- getLine
  putStrLn ("ROT13: " ++ rot13 txt)

-- b)
prop_rot13_involution :: String -> Bool
prop_rot13_involution s = rot13 (rot13 s) == s


-- 6 
type AWord = String
type Line = [AWord]
type Paragraph = [Line]

-- a)
fillWords :: Int -> [AWord] -> Paragraph
fillWords _ [] = []
fillWords maxWidth ws = line : fillWords maxWidth rest
  where
    (line, rest) = fillLine maxWidth ws

fillLine :: Int -> [AWord] -> (Line, [AWord])
fillLine _ [] = ([], [])
fillLine maxWidth (w:ws)
  | length w > maxWidth = ([w], ws)
  | otherwise           = go [w] (length w) ws
  where
    go line curLen [] = (line, [])
    go line curLen (x:xs)
      | curLen + 1 + length x <= maxWidth = go (line ++ [x]) (curLen + 1 + length x) xs
      | otherwise = (line, x:xs)

-- b)
main_fill :: IO ()
main_fill = do
  contents <- getContents
  let ws = words contents
      paragraph = fillWords 70 ws
  putStr (unlines (map unwords paragraph))


-- EX7
type Dict = [String]
-- a)
readDict :: IO Dict
readDict = do
  txt <- readFile "/usr/share/dict/words"
  return (words txt)

main_dictLength :: IO ()
main_dictLength = do
  dict <- readDict
  print (length dict)

-- b)
checkWord :: Dict -> String -> String
checkWord dict w
  | w `elem` dict = w
  | otherwise     = "\ESC[7m" ++ w ++ "\ESC[0m"

-- c)
spellCheck :: Dict -> String -> String
spellCheck dict txt =
  unlines (map checkLine (lines txt))
  where
    checkLine l = unwords (map (checkWord dict) (words l))

-- d)
main_spellCheck :: IO ()
main_spellCheck = do
  dict <- readDict
  txt <- getContents
  putStr (spellCheck dict txt)




