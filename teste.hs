penultimo :: [a] -> a
penultimo [x,y] = x
penultimo (x:xs) = penultimo xs