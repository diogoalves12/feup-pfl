-- 1.2)
leftHalf :: [a] -> [a]
leftHalf xs = take (length xs `div` 2) xs

rightHalf :: [a] -> [a]
rightHalf xs = drop (div (length xs) 2) xs
-- 1.3)
--a)
second :: [a] -> a
second xs = head (tail xs)
--b)
last1 :: [a] -> a
last1 xs = head (reverse xs)
--c)
myinit :: [a] -> [a]
myinit xs = reverse (tail (reverse xs))

myinit1 :: [a] -> [a]
myinit1 a = reverse (drop 1 (reverse a) )

myinit2 :: [a] -> [a]
myinit2 a = take ( length a - 1) a
--d)
middle0 :: [a] -> a
middle0 xs = xs !! div (length xs) 2

middle1 :: [a] -> a
middle1 xs = head (drop (div (length xs) 2) xs)
--e
check2Palindrome :: String -> Bool
check2Palindrome s = s == reverse s

-- 1.4)
checkTriangle :: Float -> Float -> Float -> Bool
checkTriangle a b c = (a < b + c) && (b < a + c) && (c < a + b)

-- 1.5)
trinangleArea :: Float -> Float -> Float -> Float
trinangleArea a b c = sqrt(s * (s - a) * (s - b) * (s -c))
                    where s = (a + b + c) / 2