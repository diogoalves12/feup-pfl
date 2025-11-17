-- 3.1)
-- a)
myand :: [Bool] -> Bool
myand [] = True
myand (x:xs) = x && myand xs

-- b)
myor :: [Bool] -> Bool
myor [] = True
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

-- f)
myelem :: Eq a => a -> [a] -> Bool
myelem _ [] = False
myelem a (x:xs) = (a == x) || myelem a xs

-- 3.2)
-- a)
leastDiv :: Integer -> Integer 
leastDiv n = ldiv n 2 where
  ldiv n d
    | d * d > n       = n
    | n `mod` d == 0  = d
    | otherwise       = ldiv n (d+1)

-- b) 
isPrimeFast :: Integer -> Bool
isPrimeFast n = n > 1 && leastDiv n == n

-- 3.3)

-- List Comprehension 
nubC :: Eq a => [a] -> [a]
nubC [] = []
nubC (x:xs) = x : [y |y <- nub xs, y/= x]

-- Recursion
nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) = x : remove x (nub xs)

remove :: Eq a => a-> [a] -> [a]
remove _ [] = []
remove n (x:xs)
  | x == n  = remove x xs
  | otherwise = x : remove n xs

-- 3.4)
intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse _ [x] = [x]
intersperse a (x:xs) = x : a : intersperse a xs

-- 3.5)
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

-- 3.6)
-- a) 
merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys)
  | x == y    = x : merge xs ys
  | x < y     = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys

-- b)
msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort left) (msort right)
        where 
          half = div (length xs) 2
          (left, right) = splitAt half xs

-- 3.7)
toBits :: Int -> [Int]
toBits 0 = [0]
toBits n = reverse (helper n)
  where 
    helper 0 = []
    helper m = mod m 2 : helper (div m 2)

-- 3.8)
fromBits :: [Int] -> Int
fromBits [] = 0
fromBits (b:bs) = b * 2 ^ length bs 

-- using lambda
fromBitsL :: [Int] -> Int
fromBitsL = foldl (\acc b -> acc * 2 + b) 0

----------------------------
-- Higher-Order Functions --
----------------------------

-- Higher-Order Functions if it takes a funtions as an argument or returns a function

{-
Predefined in the Prelude:

map
filter
takeWhile, dropWhile
all, any
foldr, foldl
-}

-- 3.9) 
divisors :: Integer -> [Integer]
divisors n = filter (\d -> mod n d == 0) [1..n]

-- 3.10)
isPrimeFastHO :: Integer -> Bool
isPrimeFastHO n = n > 1 && all (\d -> mod n d /= 0) [2..limit]
  where
    limit = floor (sqrt (fromIntegral n))

-- 3.11) 
-- a)
myAppend :: [a] -> [a] -> [a]
myAppend xs ys = foldr (:) ys xs

-- b) 
myconcatHO :: [[a]] -> [a]
myconcatHO xss = foldr (++) [] xss

-- c)
myreverseR :: [a] -> [a]
myreverseR = foldr (\x acc -> acc ++ [x]) []

-- d)
myreverseL :: [a] -> [a]
myreverseL = foldl (\acc x -> x : acc) []

-- e)
myelemHO :: Eq a => a -> [a] -> Bool
myelemHO x = any (== x) 

-- 3.12)
fromBitsHO :: [Int] -> Int
fromBitsHO = foldl (\acc b -> acc * 2 + b) 0

-- 3.13
group :: Eq a => [a] -> [[a]]
group [] = []
group (x:xs) = takeWhile (== x) (x:xs) : group (dropWhile (==x ) xs)

-- 3.14)
-- a)
intercalate :: a -> [a] -> [[a]]
intercalate x []  = [[x]]
intercalate x (y:ys) =  (x:y:ys) : map (y:) (intercalate x ys)

-- b)
permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations (x:xs) = concatMap (intercalate x) (permutations xs)