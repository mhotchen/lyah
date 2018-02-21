maximum' :: Ord t => [t] -> t
maximum' [a] = a
maximum' (a:xs) = max a (maximum' xs)

replicate' :: Int -> t -> [t]
replicate' count item
  | count <= 0 = []
  | otherwise = item : replicate' (count - 1) item

take' :: Int -> [t] -> [t]
take' n _ | n <= 0 = []
take' _ [] = []
take' count (x:xs) = x : take' (count - 1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x:repeat' x

zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs)
  | a == x = True
  | otherwise = elem' a xs
