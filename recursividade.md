```
maiorelemento :: [Int] -> Int
maiorelemento [x] = x
maiorelemento (h:t)
    | h > maiorelemento t = h
    | otherwise = maiorelemento t  
```
