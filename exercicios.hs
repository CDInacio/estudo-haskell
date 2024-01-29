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
