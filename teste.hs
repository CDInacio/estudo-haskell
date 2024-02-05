temp :: Double -> Double
temp x = (x*1.8)+32

ispar :: Int -> Bool
ispar x
    | x `mod` 2 == 0 = True
    | otherwise = False

maior :: [Int] -> Int
maior [x] = x
maior (x:xs)
    | x > maior xs = x
    | otherwise = maior xs

maior2 :: Int -> [Int] -> [Int]
maior2 n [] = []
maior2 n (x:xs)
    | x > n = x : maior2 n xs
    | otherwise = maior2 n xs

maior3 :: [Int] -> [Int]
maior3 [] = []
maior3 (x:xs)
    | x > 3 = x : maior3 xs
    | otherwise = maior3 xs

func4 :: [Int] -> [Int]
func4 [] = []
func4 (x:xs) = x : x : func4 xs


soma :: [Int] -> Int
soma xs = foldr(\x acc -> if odd x then x*x+acc else acc) 0 xs