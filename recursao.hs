divrec  :: Int -> Int -> Int
divrec x y 
    | x == y = 0
    | y > x = x
    | otherwise = divrec(x-y) y

    
multrec :: Int -> Int -> Int
multrec x n
    | x == 1 = n
    | x > 1 = n + multrec (x-1) n
    
    
potencia2 :: Int -> Int
potencia2 x
    | x == 0 = 1
    | x > 0 = 2 * potencia2 (x-1)