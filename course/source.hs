


p1 = zipWith ($) [(+1), (\x -> 2 * x + 1), (*2)] [1,2,3]
p2 = zipWith (+) [1] [1]

p3 = (\x y -> x + y)

