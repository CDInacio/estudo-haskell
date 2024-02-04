-- (1) Use o comando map para definir uma função "reverso [a]->[a]". Ele deverá retorna uma lista de elementos na ordem inversa.
swap :: [a] -> [a]
swap [] = []
swap (x:xs) = swap xs ++ [x]

inverse :: [a] -> [a]
inverse xs = swap xs

-- (2) Converta os seguintes comandos utilizando lambda: map f xs, onde f x = x * 1 + 2.
func :: [Int] -> [Int]
func xs = map (\x -> x * 1 + 2) xs

-- (3) Escreva uma função que ignore os n primeiros elementos de uma lista e retorne o restante.
ignore :: Int -> [a] -> [a]
ignore 0 xs = xs
ignore n (x:xs) = ignore (n-1) xs
-- com foldr
ignoreFold :: Int -> [a] -> [a]
ignoreFold n xs = foldr (\x acc -> if n == 0 then x:acc else ignoreFold (n-1) acc) [] xs

-- (4) Faça uma função que recebe um número e uma lista de números e retorna uma lista com os números que são maiores do que o valor informado.
maior1 :: Int -> [Int] -> [Int]
maior1 _ [] = []
maior1 n (x:xs)
 | x > n = x : maior1 n xs
 | otherwise = maior1 n xs
-- com filter
maior2 :: Int  -> [Int] -> [Int]
maior2 n xs = filter (\x -> if x > n then True else False) xs

-- (5) Faça uma função que recebe uma lista e retorna uma nova lista contendo a duplicação dos elementos da lista original. Ex: duplica [1,2,3] => [1,1,2,2,3,3]
duplica :: [a] -> [a]
duplica [] = []
duplica (x:xs) = x : x : duplica xs

-- (6) Defina uma função que recebe uma lista de números inteiros e retorna a soma dos quadrados dos números ímpares usando a função foldr.
somaquadradoimpar :: [Int] -> Int
somaquadradoimpar xs = foldr(\x acc -> if odd x then x * x + acc else acc) 0 xs

-- (7) Defina uma função que recebe uma lista de números inteiros e retorna o produto dos elementos usando a função foldr1
produtointeiro :: [Int] -> Int
produtointeiro xs = foldr1 (*) xs

-- (8) Crie uma função que recebe uma lista de palavras e retorna uma nova lista contendo o comprimento de cada palavra, utilizando a função map.
tampalavra :: [String] -> [Int]
tampalavra xs = map length xs

soma :: [Int] -> [Int]
soma xs = filter (\x -> if x `mod` 2 == 0 then True else False) xs

-- faça uma funcao que aplica (x+y)/ em uma lista
func3 x y = (x+y)/2
foldr func3 [1,2,3]
