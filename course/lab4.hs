
module Lab4 where
import Numeric.Natural
import Data.List (genericIndex)
import Data.List (find)

andFold xs = foldr (&&) True xs
sumaPatrateImpare xs = foldr op unit xs
    where
        unit = 0
        op x s 
            | odd x = x^2 + s
            | otherwise = s

-- Universalitatea foldr
medieFold xs = (foldr op unit xs) 0 0
    where
        unit = \n suma -> suma / n
        op x ac = \n suma -> ac (n+1) (suma+x)
{-|
medieFold xs = (foldr op unit xs) 0 0
    where
        unit = \n suma -> suma / n
        op x ac = \n suma -> ac (n+1) (suma+x)
|-}
medieFold xs = (foldr op unit xs) 0 0
    where
        unit n suma = suma / n
        (op x ac) n suma = ac (n+1) (suma+x)

pozitiiPareFold xs = (foldr op unit xs) 0
    where
        unit i = []
        (op a r) i 
            | even a = i : (r (i+1))
            | otherwise = (r (i+1))

-- Evaluarea lenesa

logistic :: Num a => a -> a -> Natural -> a
logistic rate start = f
  where
    f 0 = start
    f n = rate * f (n - 1) * (1 - f (n - 1)) 

logistic0 :: Fractional a => Natural -> a
logistic0 = logistic 3.741 0.00079
ex1 :: Natural
ex1 = 18

findFirstNat :: (Natural -> Bool) -> Natural
findFirstNat p = n
  where Just n = find p [0..]

-- Subterm sharing
logisticlin :: Num a => a -> a -> Natural -> a
logisticlin rate start = f
  where
    f 0 = start
    f n = rate * fni * (1 - fni)
        where fni = f (n - 1)
logisticlin0 :: Fractional a => Natural -> a
logisticlin0 = logisticlin 3.741 0.00079

-- Memoizare
-- slide 6-11 de la curs 3
genf :: Int -> Integer
genf n = l !! n
    where
        l = map f [0..]
        f 0 = 0
        f 1 = 1
        f n = (l !! (n-1)) + (l !! (n-2))

memoize :: (Natural -> a) -> (Natural -> a)
memoize f = genericIndex tabela
    where
        tabela = map f [0..]
fibonacciM ::Natural -> Natural
fibonacciM = memoize f
    where
        f 0 = 0
        f 1 = 1
        f n = fibonacciM (n-1) + fibonacciM (n-2)

catalan :: Natural -> Natural
catalan 0 = 1
catalan n = sum [ catalan i * catalan (n - 1 - i) | i <- [0..n-1] ]

catalanM :: Natural -> Natural
catalanM = memoize f
    where
        f 0 = 1
        f n = sum [ catalanM i * catalanM (n - 1 - i) | i <- [0..n-1] ]

