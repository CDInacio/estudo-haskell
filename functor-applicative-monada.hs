-- Functor -> Classe para tipos que podem ser mapeados
-- Aplica uma função em um valor envolto
-- Possui um unico método chamado fmap

-- import Data.List

-- fmap (+2) (Just 2)
-- Just 4

--  fmap (+3) [1,2,3]
-- [4,5,6]

-- (+3) <$> [1,2,3]
-- [4,5,6]

-- (+1) <$> (+2) <$> [1,2,3]
-- [4,5,6]

-- (+5) <$> (+2) <$> Just 4
-- Just 11



-- Applicatives -> Aplica funções envoltas em valores emvoltos
-- [(+3),(*2)] <*> [1,2,3]
-- [4,5,6,2,4,6]

-- Just (+3) <*> Just 3
-- Just 6

-- (+) <$> Just 5 <*> Just 5
-- Just 10


-- Mônada -> Aplica uma função que retorna um valor envolto a um valor envolto
half x = if even x then Just (x `div` 2) else Nothing
-- half 10
-- Just 5

-- Just 4 >>= half
-- Just 2

division :: Double -> Double -> Maybe Double
division x 0 = Nothing
division x y = Just (x/y)

-- division 10 0
-- Nothing

-- division 8 2
-- Just 4.0

data Foo = Foo {x::Int, y::Int} deriving (Show)

duplica :: [Int] -> [Int]
duplica [] = []
duplica (x:xs) = x * 2 : duplica xs


func :: [Int] -> [Int]
func xs = foldr (\x acc -> if x `mod` 3 == 0 then x : acc else acc) [] xs

func2 :: [Int] -> [Int]
func2 xs = foldl (\acc x -> if x `mod` 3 == 0 then x : acc else acc) [] xs

func3 :: [Int] -> [Int]
func3 xs = foldr (\x acc -> acc ++ [x]) [] xs

func4 :: [Int] -> [Int]

func4 xs = foldr (\x acc -> if odd x then x : acc else acc) [] xs

