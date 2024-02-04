-- Defina uma função que recebe uma lista de números inteiros e retorna a soma dos quadrados dos números ímpares usando a função foldr.
somaQuadradosImpares:: [Int] -> Int
somaQuadradosImpares xs = foldr (\item acc -> if odd item then item * item + acc else acc) 0 xs

somaQuadradosImpares2:: [Int] -> Int
somaQuadradosImpares2 xs = foldl (\acc item -> if odd item then item * item + acc else acc) 0 xs

-- Defina uma função que recebe uma lista de números inteiros e retorna o produto dos elementos usando a função foldr1.
produtoListaInteiros :: [Int] -> Int
produtoListaInteiros xs = foldr1 (*) xs

-- aplique a funcao (x+2) a todos os elementos de uma lista, usando map
func :: [Int] -> [Int]
func [] = []
func (x:xs) = (x+2) : func xs

-- Crie uma função que recebe uma lista de palavras e retorna uma nova lista contendo o comprimento de cada palavra, utilizando a função map.
tamStr :: String -> Int
tamStr [] = 0
tamStr (x:xs) = 1 + tamStr xs

-- Faça uma função que receba uma lista de números e retorne o maior
maior :: [Int] -> Int
maior [x] = x
maior (x:xs) 
    | x > maior xs = x
    | otherwise = maior xs

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

-- Faça uma função que calcule o fatorial de um numero
fatorial:: Int -> Int
fatorial 1 = 1
fatorial n = n * fatorial (n-1)

-- Retorna 0 se uma lista for vazia
listavazia :: [a] -> Int
listavazia xs
    | length xs == 0 = 0
    | otherwise = 1

-- Retorna o penultimo elemento de uma lista
penultimo :: [Int] -> Int
penultimo [x,_] = x
penultimo (x:xs) = penultimo xs

-- Função que assuma uma forma e retorna sua superficie
data Forma = Circulo Float Float Float | Retangulo Float Float Float Float

superficie :: Forma -> Float
superficie (Circulo _ _ r) = pi * r^2
superficie (Retangulo x1 y1 x2 y2) = (abs $ x2-x1) * (abs $ y2-y1)


half x = if even x then Just (x `div` 2) else Nothing