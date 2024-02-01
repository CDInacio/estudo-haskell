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
-- faz o de cima com foldr
ignoreFold :: Int -> [a] -> [a]
ignoreFold n xs = foldr (\x acc -> if n == 0 then x:acc else ignoreFold (n-1) acc) [] xs