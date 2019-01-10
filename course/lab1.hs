

import Data.List
import Test.QuickCheck

a = permutations [1,2,3]

double x = x+x
triple x = x+x+x
penta x = 5*x


test x = (double x + triple x) == (penta x)

maxim x y =
    if( x>y )
        then x
        else y

maxim3 x y z =
    let u = maxim x y
    in maxim u z

data Alegere = Piatra | Foarfeca | Hartie deriving (Eq, Show)
data Rezultat = Victorie | Infrangere | Egalitate deriving (Eq, Show)

partida :: Alegere -Alegere -Rezultat
partida a b
    | a == Hartie && b == Piatra = Victorie
    | a == Piatra && b == Hartie = Infrangere
    | otherwise = Egalitate

