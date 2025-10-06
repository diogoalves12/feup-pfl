{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use !!" #-}


leftHalf :: [a] -> [a]
leftHalf arr = take (div (length arr) 2) arr
    
rigthHalf :: [a] -> [a]
rigthHalf arr = drop (div (length arr) 2) arr

second :: [a] -> a
second a = head (tail a)

last :: [a] -> a
last a = head (reverse a)

init :: [a] -> [a]
init a = reverse (drop 1 (reverse a) )

init1 :: [a] -> [a]
init1 a = take (length a - 1) a 

middle :: [a] -> a 
middle a = head (drop (div (length a) 2) a)

checkPalindrome :: String -> Bool
checkPalindrome res = res == reverse res

-- Era float mas da erro.
checkTriangle :: Int -> Int -> Int -> Bool
checkTriangle a b c = (a < b + c) && (b < a + c) && (c < a + b)

triangleArea :: Float -> Float -> Float -> Float
triangleArea a b c = sqrt (s * (s - a) * (s - b) * (s - c))
  where s = (a + b + c) / 2