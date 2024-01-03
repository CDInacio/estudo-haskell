------------------------------------------- Básico -------------------------------------------


-- função soma que recebe dois inteiros e retorna um inteiro
somaInt :: Int -> Int -> Int
somaInt x y = x + y

-- função soma que recebe dois float e retorna um float
somaFloat :: Float -> Float -> Float
somaFloat a b = a + b

-- função polinomio que recebe inteiro e retorna inteiro
polinomio :: Int -> Int
polinomio x = x*x + 10*x +  2

-- função quadrado que recebe inteiro e retorna inteiro
quadrado :: Int -> Int 
quadrado x = x*x

-- função triplica que recebe inteiro e retorna inteiro
triplica :: Int -> Int
triplica x = x*3

-- função areaCirculo que recebe float e retorna float
areaCirculo :: Float -> Float
areaCirculo raio = pi * raio*raio


------------------------------------------- Condicionais -------------------------------------------
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
  

------------------------------------------- Recursão -------------------------------------------
divrec  :: Int -> Int -> Int
divrec x y 
    | x == y = 0
    | y > x = x
    | otherwise = divrec(x-y) y

    
multrec :: Int -> Int -> Int
multrec x n
    | x == 1 = n
    | x > 1 = n + multrec (x-1) n
    
    
poentncia2 :: Int -> Int
potencia2 x
    | x == 0 = 1
    | x > 0 = 2 * potencia (x-1)
    

------------------------------------------- Listas -------------------------------------------
comprimento :: [Int] ->  Int
comprimento [] = 0
comprimento (h:t) = 1 + comprimento t 

cubo :: Int -> Int
cubo x = x * x * x 

aoCubo :: [Int] -> [Int]
aoCubo [] = []
aoCubo (h:t) = cubo(h) : aoCubo t

somatorio :: [Int] -> Int   
somatorio [] = 0
somatorio (h:t) = h + somatorio t