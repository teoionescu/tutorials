

import           Data.List     (sort)
 
type Linie = Integer
type Coloana = Integer

type Partida = [(Linie, Coloana)]
 
exemplu1 :: Partida
exemplu1 = [ (2, 2), (1, 3), (2, 3), (2, 4), (3, 5), (0, 2), (2, 1), (1, 4)
           , (2, 0), (1, 2), (3, 1), (1, 0)
           ]

test11 :: Bool
test11 = 
  separaX0 exemplu1
  ==  ( [(2,2),(2,3),(3,5),(2,1),(2,0),(3,1)]
      , [(1,3),(2,4),(0,2),(1,4),(1,2),(1,0)])
 
separaX0 :: [a] -> ([a], [a])
separaX0 xs = ([fst y | y <- t, mod (snd y) 2 == 0], [fst y | y <- t, mod (snd y) 2 == 1]) where t = zip xs [0..]

test12 :: Bool
test12 = 
  maxLista [[1,2,3], [4,5], [6], [7, 8, 9, 10], [11, 12, 13]]
  == [7, 8, 9, 10]
 
maxLista :: [[a]] -> [a]
maxLista xs = foldr f [] xs where
    f a b
        | (length a) > (length b) = a
        | otherwise = b

test130, test13X :: Bool
test130 =
  grupeazaUnite (sort [(1,3),(2,4),(0,2),(1,4),(1,2),(1,0)])
  == [[(0,2)],[(1,0)],[(1,2),(1,3),(1,4)],[(2,4)]]
test13X =
  grupeazaUnite (sort [(2,2),(2,3),(3,5),(2,1),(2,0),(3,1)])
  == [[(2,0),(2,1),(2,2),(2,3)],[(3,1)],[(3,5)]]

grupeazaUnite :: Partida -> [Partida]
grupeazaUnite [x] = [[x]]
grupeazaUnite ((a, b) : (c, d) : xs) =
    if a == c && d == b + 1
        then ((a, b) : head y) : (tail y)
        else [(a, b)] : y
    where y = grupeazaUnite ((c, d) : xs)

test14 :: Bool
test14 =
  maxInLinie exemplu1 == ([(2,0),(2,1),(2,2),(2,3)], [(1,2),(1,3),(1,4)])
 
maxInLinie :: Partida -> (Partida, Partida)
maxInLinie p = let (x, o) = separaX0 p; f = maxLista . grupeazaUnite . sort in (f x, f o)

------------------------------------------------------------------------

data Binar a = Gol | Nod (Binar a) a (Binar a)

exemplu2 :: Binar (Int, Float)
exemplu2 =
  Nod 
      (Nod (Nod Gol (2, 3.5) Gol) (4, 1.2) (Nod  Gol (5, 2.4) Gol))
      (7, 1.9)
      (Nod Gol (9, 0.0) Gol)

data Directie = Stanga | Dreapta

type Drum = [Directie]

test211, test212 :: Bool
test211 = plimbare [Stanga, Dreapta] exemplu2 == Just (5, 2.4)
test212 = plimbare [Dreapta, Stanga] exemplu2 == Nothing

plimbare :: Drum -> Binar a -> Maybe a
plimbare [] (Nod _ value _) = Just value
plimbare _ Gol = Nothing
plimbare (Stanga : xs) (Nod left _ _) = plimbare xs left
plimbare (Dreapta : xs) (Nod _ _ right) = plimbare xs right

type Cheie = Int
type Valoare = Float

newtype WriterString a = Writer { runWriter :: (a, String) }

instance Monad WriterString where
  return x = Writer (x, "")
  ma >>= k =  let (x, logx) = runWriter ma
                  (y, logy) = runWriter (k x)
              in  Writer (y, logx ++ logy)

tell :: String -> WriterString ()
tell s = Writer ((), s)
 
instance Functor WriterString where
  fmap f mx = do { x <- mx ; return (f x) }
 
instance Applicative WriterString where
  pure = return
  mf <*> ma = do { f <- mf ; a <- ma ; return (f a) }

test221, test222 :: Bool
test221 = runWriter (cauta 5 exemplu2) == (Just 2.4, "Stanga; Dreapta; ")
test222 = runWriter (cauta 8 exemplu2) == (Nothing, "Dreapta; Stanga; ")
 
cauta :: Cheie -> Binar (Cheie, Valoare) -> WriterString (Maybe Valoare)
cauta _ Gol = return Nothing
cauta c (Nod left val right)
    | c == (fst val) = do
        return (Just 2.4)
    | c < (fst val) = do
        tell "Stanga; "
        cauta c left
    | otherwise = do
        tell "Dreapta; "
        z <- cauta c right
        return z



-- https://www.youtube.com/watch?v=ZhuHCtR3xq8
-- :k M
-- M :: * -> *
newtype M a = M { unM :: a } deriving (Eq, Show)
instance Monad M where
    -- return ::  a -> M a
    return x  = M x
    -- bind   ::  (a -> M b) -> (M a -> M b)
    -- (>>=)  ::  M a -> (a -> M b) -> M b
    ma >>= k  = let a = unM ma
                    b = unM (k a)
                in M b
f = (+2)
g = (^2)
h = (id)
t = f . g . h
induce :: (a -> b) -> (a -> M b)
induce f a = M (f a)
f' = induce f
g' = induce g
h' = induce h
t' = \x0 ->
        h' x0 >>= \x1 ->
        g' x1 >>= \x2 ->
        f' x2
testEquiv x = (t' x) == (return (t x))
t'' = \x0 -> do
    x1 <- h' x0
    x2 <- g' x1
    x3 <- f' x2
    return x3

instance Functor M where
    fmap f mx = do { x <- mx ; return (f x) }
instance Applicative M where
    pure = return
    mf <*> ma = do { f <- mf ; a <- ma ; return (f a) }
