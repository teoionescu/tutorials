

factori n = [x | x <- [1..n], n `rem` x == 0]
prim n = length (factori n) == 2
numerePrime n = [x | x <- [1..n], prim x]

-- Ciur
clearciur x xs = [p | p<-xs, p `rem` x /= 0]
ciur [] = []
ciur (x:xs) = x : l
    where l = ciur (clearciur x xs)
numerePrimeCiur n = ciur [2..n]

-- Ordonata zip and
ordonataNat [] = True
ordonataNat [x] = True
ordonataNat ( x : xs ) = and [ k | let t = zip (x:xs) xs, p <- t, let k = fst p < snd p ]

-- Functii ca argument
(*<*) :: (Integer, Integer) -> (Integer, Integer) -> Bool

(*<*) (a, b) (c, d) = 
    if (a == c) then b < d
    else a < c

a = map ($3) [(4+), (10*), (^2), sqrt]

f pt = \x -> elem x pt
aa = map (f [2,3]) [1,3,4,5]

prel2 xs = map (\x -> if (x `mod` 2 == 0) then x `div` 2 else 2 * x) xs
-- prel2 xs = map (\x -case () of
--    _ | (x `mod` 2 == 0) -x `div` 2 
--      | otherwise -2 * x) xs

-- Compunere functii
compuneList f xs = map (f.) xs
aplicaList x xs = map ($x) xs
aaa = aplicaList 9 (compuneList (+1) [ sqrt, (^2), (/2) ])

-- Myzip3
myzip3 as bs cs = map (\(x, y) -> (x, fst y, snd y)) (zip as (zip bs cs))

-- Filter
numaiVocale xs = map (filter (`elem` "aeiouAEIOU")) xs
aaaa = numaiVocale ["laboratorul" , "PrgrAmare" , "DEclarativa"]