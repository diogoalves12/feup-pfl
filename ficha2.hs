{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use guards" #-}

-- EX1
-- Using guards
classify :: Int -> String
classify x | x <= 9  = "failed"
           | x <= 12 = "passsed"
           | x <= 15 = "good"
           | x <= 18 = "very good"
           | x <= 20 = "excellent"

-- Using conditional expressions
classify1 :: Int -> String
classify1 x = if x <= 9 then "failed" else
              if x <= 12 then "passed" else
              if x <= 15 then "good" else
              if x <= 18 then "very good" else "excellent"

-- EX2 
classifyBMI :: Float -> Float -> String
classifyBMI w h | bmi < 18.5 = "underweight"
                | bmi < 25 = "normal weight"
                | bmi < 30 = "overweight"
                | bmi > 30 = "obese"
       where bmi = w / h^2

-- EX3
max3 :: Ord a => a -> a -> a -> a
max3 x y z | x >= y && x >= z = x
           | y >= x && y >= z = y
           | otherwise = z

min3 :: Ord a => a -> a -> a -> a
min3 x y z | x <= y && x <= z = x
           | y <= x && y <= z = y
           | otherwise = z

--EX4 
xor :: Bool -> Bool -> Bool
xor x y | x /= y = True
xor _ _ = False


--EX5
-- Using conditionals expressions
safetailC :: [a] -> [a]
safetailC xs = if null xs then [] else tail xs

--Using guards
safetailG :: [a] -> [a]
safetailG xs | null xs = []
             | otherwise = tail xs

-- Using patterns 
safetailP :: [a] -> [a]
safetailP [] = []
safetailP xs = tail xs

--EX6
--Using length function
shortC :: [a] -> Bool
shortC xs = length xs <= 3 

-- Using patterns 
shortP :: [a] -> Bool
shortP (_:(_:(_:_))) = False
shortP _ = True

-- EX7
-- Conditions
median :: Ord a => a -> a -> a -> a
median x y z
  | (x <= y && y <= z) || (z <= y && y <= x) = y
  | (y <= x && x <= z) || (z <= x && x <= y) = x
  | otherwise = z

-- Sum all values and take the diff between the max and min values.
medianS :: (Ord a, Num a) => a -> a -> a -> a
medianS x y z = x + y + z - maximum [x,y,z] - minimum [x,y,z]

-- LIST COMPREHENSION : [ expressão | gerador(es), condição(ões)]

-- EX8
propDivs :: Integer -> [Integer]
propDivs xs = [x | x <- [1..xs - 1], xs `mod`x == 0 ]

-- EX9
perfects ::  Integer -> [Integer]
perfects limit = [n | n <- [1..limit], sum(propDivs n) == n] 

-- EX10
pyths :: Integer -> [(Integer,Integer,Integer)]
pyths limit =  [(x, y ,z) | x <- [1..limit], y <- [1..limit], z <- [1..limit], x^2 + y^2 == z^2]

-- EX11
-- aux function
divisors :: Integer -> [Integer]
divisors xs = [x | x <- [1..xs], xs `mod`x == 0 ]

isPrime :: Integer -> Bool
isPrime n = divisors n == [1, n]

-- EX12
myconcat :: [[a]] -> [a]
myconcat xss = [x | xs <- xss, x <- xs]

myreplicate :: Int -> a -> [a]
myreplicate n v = [v | _ <- [1..n]]

myindex :: [a] -> Int -> a
myindex xs n = head [x | (i,x) <- zip [0..] xs, i == n]

-- EX13

fact :: Integer -> Integer
fact n = product [1..n]

binom :: Integer -> Integer -> Integer
binom n k = fact n `div` (fact k * fact (n-k))

pascal :: Integer -> [[Integer]]
pascal n = [[binom r k | k <- [0..r]] | r <- [0..n]]
