-- Usando if then else
    maior :: Int -> Int -> Int
    maior a b = if a > b then a else b
    
    -- Usando guardas
    maior2 :: Int -> Int -> Int
    maior2 a b
        | a > b = a
        | a < b = b
        | otherwise = 0 
        
    
    -- Fatorial
    fatorial :: Int -> Int
    fatorial x 
        | x == 0 = 1
        | x > 0 = x * fatorial (x-1)
    
    
    -- Retorna se um número é par ou não (usnado if then else)
    ehPar :: Int -> Bool
    ehPar x = if mod 2 x == 0 then True else False
    
    
    -- Retorna se um número é par ou não (usnado guardas)
    ehPar2 :: Int -> Bool
    ehPar2 x
        | mod 2 x == 0 = True
        | otherwise = False
    
    
    -- Retorna se um char é maiusculo ou minusculo
    charCase :: Char -> String
    charCase x
        | x >= 'a' && x <= 'z' = "minusculo"
        | x >= 'A' && x <= 'Z' = "maiusculo"
        | otherwise = "nao eh uma letra"
    
    
    -- Funcao que recebe 3 inteiros e retorna uma conta
    funcao :: Int -> Int -> Int -> Int
    funcao x y z
        | x == 0 = y*y - 3*z
        | x == 1 = 2*z^2 - 3*x
        | x ==  2 = 3*z - y
        | otherwise = 0
      