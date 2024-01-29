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