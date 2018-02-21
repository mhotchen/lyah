quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
  let lte = [a | a <- xs, a <= x]
      gt  = [a | a <- xs, a  > x]
  in quicksort lte ++ [x] ++ quicksort gt
