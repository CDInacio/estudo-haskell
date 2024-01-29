comprimento :: [Int] ->  Int
comprimento [] = 0
comprimento (x:xs) = 1 + comprimento xs


cubo :: Int -> Int
cubo x = x * x * x 


aoCubo :: [Int] -> [Int]
aoCubo [] = []
aoCubo (x:xs) = cubo(x) : aoCubo xs


somatorio :: [Int] -> Int   
somatorio [] = 0
somatorio (x:xs) = x + somatorio xs

-- somatorio (1:[2,3])
-- = 1 + somatorio [2,3] -> 1 + 5 = 6
-- = 2 + somatorio [3] -> 2 + 3 = 5
-- = 3 + somatorio [] ->  3 + 0 = 3
-- = somatorio [] = 0


possuichar :: [Char] -> Char -> Bool
possuichar [] char = False
possuichar (x:xs) ch
    | x == ch = True
    | otherwise = possuichar xs ch


maiorelemento :: [Int] -> Int
maiorelemento [x] = x
maiorelemento (x:xs)
    | x > maiorelemento xs = x
    | otherwise = maiorelemento xs  
