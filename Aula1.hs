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
    
    
potencia2 :: Int -> Int
potencia2 x
    | x == 0 = 1
    | x > 0 = 2 * potencia2 (x-1)
    

------------------------------------------- Listas -------------------------------------------
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



------------------------------ Funçoes de alta ordem ------------------------------
-- Funções de alta ordem são funções que podem receber outras funções como argumentos
-- e/ou retornar funções como resultados.

-- Função de mapeamento
mapInt:: (Int->Int) -> [Int] -> [Int]
mapInt _ [] = []
mapInt f (x:xs) = (f x) : mapInt f xs

-- função map nativa do haskell


-- Função de filtro
pares :: Int  -> Bool
pares x = (mod x 2 == 0)

filtro :: (Int->Bool) -> [Int] -> [Int]
filtro f [] = []
filtro f (x:xs) 
    | (f x) == True = x : filtro f xs
    | otherwise = filtro f xs


-- busca
maiorNum :: Int -> Int -> Bool
maiorNum x y = if x > y then True else False

buscaLista :: (Int->Int->Bool) -> [Int] -> Int
buscaLista f [x] = x
buscaLista f (x:xs)
    | f x (buscaLista f xs) = x
    | otherwise = buscaLista f xs




------------------------------ Fold ------------------------------
-- Foldr é uma função de alta ordem que recebe uma função, um valor inicial e uma lista
-- e retorna um valor único, que é o resultado da aplicação da função nos elementos da lista
-- e no valor inicial.

foldR :: (a -> b -> b) -> b -> [a] -> b
foldR f v [] = v
foldR f v (x:xs) = f x (foldR f v xs)

tam :: [a] -> Int
tam xs = foldR (\a b -> 1+b) 0 xs

reverso :: [a] -> [a]
reverso xs = foldR (\x xs -> xs ++ [x]) [] xs
 
-- Foldl é uma função de alta ordem que recebe uma função, um valor inicial e uma lista
-- e retorna um valor único, que é o resultado da aplicação da função nos elementos da lista
-- e no valor inicial.

foldL :: (b -> a -> b) -> b -> [a] -> b
foldL f v [] = v
foldL f v (x:xs) = foldL f (f v x) xs

tam2 :: [a] -> Int
tam2 xs = foldL (\b a -> 1+b) 0 xs

reverso2 :: [a] -> [a]
reverso2 xs = foldL (\xs x -> x:xs) [] xs


------------------------------ Tipo Algebrico ------------------------------

-- data significa que estamos definindo um novo tipo de dados
data Bool = False | True
-- A parte antes de = denota o tipo, as partes depois de = são construtores de valor, eles especificam os diferentes valores que esse tipo pode ter
--Ex:
data Forma = Circulo Float Float Float | Retangulo Float Float Float Float

superficie :: Forma -> Float
superficie (Circulo _ _ r) = Pi * r^2
superficie (Retangulo x1 x2 y1 y2) = (abs $ x2-x1) * (abs $ y2-y1)


------------------------------ Classes e Instancias ------------------------------
data Foo = Foo {x: Integer, str: String}
instance Eq Foo where
        (Foo x1 str1) == (Foo x2 str2) = (x1==x2) && (str1 == str2)

-- Assim, podemos fazer:
-- Foo 1 "ola" /= Foo 1 "hello"
-- True

-- Foo 1 "comida" == Foo 1 "carro"
-- False


-- Derivação
data Foo2 = Foo2 {x: Integer, str: String} deriving (Eq, Show, Ord)

-- Verificar se dois circulos são iguais

-- data Forma = Circulo Float Float Float | Retangulo Float Float Float Float deriving (Eq, Show, Ord)

-- let circulo1 = Circulo 1.2 1.2 1.3
-- let circulo2 = Circulo 1.2 1.2 1.4
-- > circulo1 == circulo2
-- False


------------------------------ Exercicios ------------------------------

-- Defina uma função que recebe uma lista de números inteiros e retorna a soma dos quadrados dos números ímpares usando a função foldr.
somaQuadradosImpares:: [Int] -> Int
somaQuadradosImpares xs = foldr (\item acc -> if odd item then item * item + acc else acc) 0 xs

-- Defina uma função que recebe uma lista de números inteiros e retorna o produto dos elementos usando a função foldr1.
produtoListaInteiros :: [Int] -> Int
produtoListaInteiros xs = foldr1 (*) xs

-- Crie uma função que recebe uma lista de palavras e retorna uma nova lista contendo o comprimento de cada palavra, utilizando a função map.
tamStr :: String -> Int
tamStr [] = 0
tamStr (x:xs) = 1 + tamStr xs

comprimentoPalavras:: [String] -> [Int]
comprimentoPalavras xs = map length xs
-- assim tbm funciona
-- comprimentoPalavras xs = map length xs

-- Aplique a função raiz quadrada em uma lista
calculaRaiz :: [Float] -> [Float]
calculaRaiz xs = foldr(\item acc -> sqrt item : acc) [] xs

-- Escreva uma função map que retorne os itens de uma lista ao contrario
reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

mapReverse :: [[a]] -> [[a]]
mapReverse xs = map reverseList xs

