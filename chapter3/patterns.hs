lucky :: Int -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky _ = "Sorry, you're out of luck, pal!"

fact :: Int -> Int
fact 0 = 1
fact i = i * fact(i - 1)

addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

head' :: [a] -> a
head'[] = error "Can't call head on an empty list, dummy!"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell all@(x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y ++ " and has " ++ show (length all) ++ " elements"

bmiTell :: Double -> Double -> String
bmiTell weight height
  | bmi <= underweight = "You're underweight"
  | bmi <= normal = "You're normal"
  | bmi <= overweight = "You're overweight"
  | otherwise   = "You're morbidly obese"
  where
      bmi = weight / height ^ 2
      underweight = 18.5
      normal = 25.0
      overweight = 30.0

calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [ w / h ^ 2 | (w, h) <- xs ]
