doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber x = if x > 100
                      then x
                      else doubleMe x
doubleSmallNumber' x = (if x > 100 then x else doubleMe x) + 1
conanO'Brien = "It's a-me, Conan O'Brien!"

boomBangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

let triangles = [ (a, b, c) | a <- [1..11], b <- [1..11], c <- [1..11] ]
let rightAngleTrinagles = [ (a, b, c) | c <- [1..10], a <- [1..c], b <- [1..a], a^2 + b^2 == c^2, a+b+c == 24]
