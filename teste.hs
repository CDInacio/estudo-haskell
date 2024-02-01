-- aplique a funcao (x+2) a todos os elementos de uma lista, usando map
func :: [Int] -> [Int]
func xs = foldr (\x acc -> x + 2 : acc) [] xs

--
func2 :: [Int] -> [Int]
func2 xs = map (\x -> x * 1 + 2) xs