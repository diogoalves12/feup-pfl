-- 2.1)
-- Using guards:
classify :: Int -> String
classify x | x <= 9  = "failed"
           | x <= 12 = "passed"
           | x <= 15 = "good"
           | x <= 18 = "very good"
           | x <= 20 = "excellent"

-- Using conditional expressions:
classifyC :: Int -> String
classifyC x = if x <= 9 then "failed" else 
              if x <= 12 then "passed" else 
              if x <= 15 then "good" else 
              if x <= 18 then "very good" 
              else "excellent "

-- 2.2)
classifyBMI :: Float -> Float -> String
classifyBMI w h 
    | bmi < 18.5 = "underweight"
    | bmi < 25 = "normal weight"
    | bmi < 30 = "overweight"
    | bmi >= 30 = "obese"
    where bmi = w / h^2

-- 2.3)
max3 :: Ord a => a -> a -> a -> a
max3 x y z 
    | x >= y && x >= z = x
    | y >= x && y >= z = y
    | otherwise = z

min3 :: Ord a => a -> a -> a -> a
min3 x y z 
    | x <= y && x <= z = x
    | y <= x && y <= z = y
    | otherwise = z

-- Using association
max3A :: Ord a => a -> a -> a -> a
max3A x y z = max x (max y z)
-- or max3A x y z = max (max x y) z

min3A :: Ord a => a -> a -> a -> a
min3A x y z = min x (min y z)
-- oe min3A x y z = min (min x y) z

-- 2.4)
xor :: Bool -> Bool -> Bool
xor a b | a /= b = True
xor _ _ = False

xor1 :: Bool -> Bool -> Bool
xor1 True  b = not b
xor1 False b = b

-- 2.5)
-- Using conditional expressions
safetail :: [a] -> [a]
safetail xs = if null xs then [] else tail xs

-- Using guards 
safetailG :: [a] -> [a]
safetailG xs 
    | null xs = []
    | otherwise = tail xs

-- Using patterns 
safetailP :: [a] -> [a]
safetailP [] = []
safetailP xs = tail xs
-- or 
safetailP1 :: [a] -> [a]
safetailP1 [] = []
safetailP1 (_:xs) = xs      -- Note: (_:xs) -> _ takes first element of xs and returns xs without _

-- 2.6)
short :: [a] -> Bool
short xs = length xs < 3

shortEP :: [a] -> Bool
shortEP (_:(_:(_:_))) = False
shortEP _ = True
-- or 
shortEP1 :: [a] -> Bool
shortEP1 []        = True          -- 0 elements
shortEP1 [_]       = True          -- 1 elements
shortEP1 [_, _]    = True          -- 2 elements
shortEP1 _         = False         -- 3 or more elements

-- 2.7)
median :: Ord a => a -> a -> a -> a
median x y z
    | (x >= y && x <= z) || (x >= z && x <= y) = x
    | (y >= x && y <= z) || (y >= z && y <= x) = y
    | (z >= x && z <= y) || (z >= y && z <= x) = z

medianS :: (Num a, Ord a) => a -> a -> a -> a
medianS x y z = s - maximum [x,y,z] - minimum [x,y,z]
                where s = x + y + z 
-- or
medianS1 :: (Ord a, Num a) => a -> a -> a -> a
medianS1 x y z = x + y + z - maximum [x,y,z] - minimum [x,y,z]

{-

LIST COMPREHENSION : [ expressão | gerador(es), condição(ões)]

-}

-- 2.8)
propDivs :: Integer -> [Integer]
propDivs n = [d | d <- [1..n - 1], n `mod` d == 0]

-- 2.9)
perfects :: Integer -> [Integer]
perfects l = [n | n <- [1..l], sum(propDivs n) == n]

-- 2.10)
pyths :: Integer -> [(Integer,Integer,Integer)]
pyths l = [(x,y,z) | x <- [1..l], y <- [1..l], z <- [1..l], x^2 + y^2 == z^2] 

-- 2.11)
isPrime :: Integer -> Bool
isPrime n = divisors == [1,n]
            where divisors = [d | d <- [1..n], mod n d == 0]
-- or using aux function 
auxdivisors :: Integer -> [Integer]
auxdivisors xs = [x | x <- [1..xs], xs `mod`x == 0 ]

isPrime1 :: Integer -> Bool
isPrime1 n = auxdivisors n == [1, n]

-- 2.12) 
myconcat :: [[a]] -> [a]
myconcat xss = [x | xs <- xss, x <- xs] -- xss = list of lists, xs = list in xss, x= element in xs

myreplicate :: Int -> a -> [a]
myreplicate n x = [x | _  <- [1..n]]

myindex :: [a] -> Int -> a
myindex xs n = head [x | (x,i) <- zip xs [0..], i == n]

-- 2.13)

-- a)
-- aux factorial func
fact :: Integer -> Integer
fact n = product[1..n]

binon :: Integer -> Integer -> Integer
binon n k = fact n `div` (fact k * fact (n - k))

-- b)
pascal :: Integer -> [[Integer]]
pascal l = [[binon n k | k <- [0..n]] | n <- [0..l]]