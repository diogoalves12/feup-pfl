{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}

{-
Observations:

- The first equation (base case) defines the factorial of 0
-The second equation (recursive case) defines the factorial of n using the factorial of n − 1
    
If we start from a positive integer, the recursive case makes progress towards the base case 
Hence: factorial is defined for all non-negative integers

If we start from a negative integer, the recursive case moves away from the base case
Hence: factorial (-1) leads to non-termination

The order of the two equations matters:
    factorial n = n * factorial (n-1)
    factorial 0 = 1

The second equation is never tried, hence this version is not defined for any integer!
-}


-- EX1

-- a)
myand :: [Bool] -> Bool
myand [] = True
myand (x:xs) = x && myand xs

-- b)
myor :: [Bool] -> Bool
myor [] = False                 
myor (x:xs) = x || myor xs

-- c)
myconcat :: [[a]] -> [a]
myconcat [] = []                
myconcat (xs:xss) = xs ++ myconcat xss

-- d)
myreplicate :: Int -> a -> [a]
myreplicate 0 _ = []           
myreplicate n x = x : myreplicate (n-1) x 

-- e)
myindex :: [a] -> Int -> a
myindex (x:_) 0 = x
myindex (_:xs) n = myindex xs (n-1)  
myindex [] _ = error "out of bounds"

-- f)
myelem :: Eq a => a -> [a] -> Bool
myelem _ [] = False               
myelem y (x:xs) = (y == x) || myelem y xs 

-- EX2
-- a)
leastDiv :: Integer -> Integer
leastDiv n = lDiv n 2  where 
    lDiv n d
      | d * d > n       = n
      | n `mod` d == 0  = d
      | otherwise       = lDiv n (d + 1)

-- b)
isPrimeFast :: Integer -> Bool
isPrimeFast n = n > 1 && leastDiv n == n


-- EX3
-- Recursion and list comprehension 
nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) = x : [y | y <- nub xs , y /= x] 

-- Only with recursion
nub1 :: Eq a => [a] -> [a]
nub1 [] = []
nub1 (x:xs) = x : remove x (nub1 xs)

remove :: Eq a => a -> [a] -> [a]
remove _ [] = []
remove x (y:ys)
  | x == y    = remove x ys
  | otherwise = y : remove x ys


-- EX4
intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse _ [x] = [x]
intersperse a (x:y:xs) = x : a : intersperse a (y:xs)

-- EX5
-- a)
insert :: Ord a => a -> [a] -> [a]
insert a [] = [a]
insert a (x:xs) 
    | a <= x    = a : x : xs 
    | otherwise = x : insert a xs

-- b)
isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)

-- EX6
-- a)
merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge [] xs = xs
merge  xs [] = xs
merge (x:xs) (y:ys) 
    | x == y    = x : merge xs ys
    | x < y    = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

{------------------------------------ REVER(All BELOW) ------------------------------------}
-- b)
msort :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x]
msort xs  = merge (msort left) (msort right)
  where
    half = length xs `div` 2
    (left, right) = splitAt half xs

-- EX7
toBits :: Int -> [Int]
toBits 0 = [0]
toBits n = reverse (helper n)
  where
    helper 0 = []
    helper m = (m `mod` 2) : helper (m `div` 2)

-- EX8
fromBits :: [Int] -> Int
fromBits = foldl (\acc b -> acc * 2 + b) 0


-- Higher-Order Functions

-- EX9
divisors :: Integer -> [Integer]
divisors n = filter (\d -> n `mod` d == 0) [1..n]

-- EX10
isPrimeFast2 :: Integer -> Bool
isPrimeFast2 n = n > 1 && all (\d -> n `mod` d /= 0) [2 .. limit]
  where
    limit = floor (sqrt (fromIntegral n))

-- EX11
-- a)
myAppend :: [a] -> [a] -> [a]
myAppend xs ys = foldr (:) ys xs

-- b)
myConcat :: [[a]] -> [a]
myConcat = foldr (++) []

-- c)
myReverseR :: [a] -> [a]
myReverseR = foldr (\x acc -> acc ++ [x]) []

-- d)
myReverseL :: [a] -> [a]
myReverseL = foldl (\acc x -> x : acc) []

-- e)
myElem :: Eq a => a -> [a] -> Bool
myElem x = any (== x)

-- EX12
fromBits2 :: [Int] -> Int
fromBits2 = foldl (\acc b -> acc * 2 + b) 0

-- EX13
group :: Eq a => [a] -> [[a]]
group [] = []
group (x:xs) = (x : takeWhile (== x) xs) : group (dropWhile (== x) xs)

-- EX14
-- a)
intercalate :: a -> [a] -> [[a]]
intercalate x []     = [[x]]
intercalate x (y:ys) = (x:y:ys) : map (y:) (intercalate x ys)

-- b)
permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations (x:xs) = concatMap (intercalate x) (permutations xs)




