
import Test.QuickCheck

-- Fibonacci
fib a
    | a == 0 = 0
    | a == 1 = 1
    | otherwise = (fib (a-1)) + (fib (a-2))

f n a b
    | n == 2 = b
    | otherwise = f (n-1) b (a+b)

fibl 0 = 0
fibl 1 = 1
fibl n = f n 1 1

fibonacciLiniar :: Int -> Integer
fibonacciLiniar 0 = 0
fibonacciLiniar n = snd (fibonacciPereche n)
  where
    fibonacciPereche :: Int -> (Integer, Integer)
    fibonacciPereche 1 = (0, 1)
    fibonacciPereche n = (snd prev, fst prev + snd prev)
        where prev = fibonacciPereche (n-1)

fibmap = (map fib' [0 ..] !!)
    where
      fib' 0 = 0
      fib' 1 = 1
      fib' n = fibmap (n - 1) + fibmap (n - 2)

propfib :: Int -> Property
propfib n = (n >= 0 && n <= 100) ==> (fibonacciLiniar n == fibl n) && (fibl n == fibmap n)

-- List recursion
ininterval :: Int -> Int -> [Int] -> [Int]
ininterval _ _ [] = []
ininterval a b (h : t)
    | (a <= h && h <= b) = h : pt
    | otherwise = pt
    where pt = ininterval a b t

-- List comprehension
a = [(i,j) | i <- [1,2], j <- [1..4]]

inintervalcom a b xs
        = [ (i) | i <- xs, a<=i, i<=b ]

testt :: Int -> Int -> [Int] -> Property
testt a b xs = (a <= b) ==> ininterval a b xs == inintervalcom a b xs

pozitive xs = length [ x | x <- xs, x 0 ]


pozitiiimpare xs = [ p | xab <- zip xs [0..], mod (fst xab) 2 == 1, let p = snd xab ]

aa = pozitiiimpare [0, 1, (-3), (-2), 8, (-1), 6, 1]

-- Directions
data Directie = Nord | Sud | Est | Vest deriving (Eq, Show)
type Punct = (Int, Int)
-- Origine :: Punct
origine :: Punct
origine = (0, 0)
miscare :: Punct -> Directie -> Punct
miscare (a, b) d
        | d == Nord = (a, b+1)
        | d ==  Est = (a+1, b)
        | d ==  Sud = (a, b-1)
        | d == Vest = (a-1, b)
type Drum = [Directie]
move :: Punct -> Drum -> Punct
move o [] = o
move o ds =
    let o' = miscare o (head ds)
    in move o' (tail ds)
