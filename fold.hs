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
