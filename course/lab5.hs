-- Timp de lucru: 1 ora
-- La sfarsitul fisierului gasiti o lista de functii ajutatoare  

import Data.Char
import Test.QuickCheck
 
type Cifra = Int
type Numar = [Cifra]

-- In acest test vom implementa cateva operatii pe numere mari.
-- O Cifra este un numar intreg intre 0 si 9.
-- Un Numar este o lista de Cifre. E.g., [2,1,4]
-- Numarul intreg reprezentat de un Numar n este obtinut
-- prin alipirea cifrelor lui n de la stanga la dreapta,
-- ignorand cifrele de 0 de la inceputul numarului.
-- E.g., numarul corespunzator lui [0, 0, 0, 2, 1, 4] este 214.
-- Prin conventie lista vida de cifre [] poate reprezenta nr. 0


-- 1a) Scrieti o functie care date fiind un Numar n si o lungime l,
-- adauga l cifre de 0 la stanga lui n.
-- E.g., lungimePlus [2, 1, 4] 3 = [0, 0, 0, 2, 1, 4]
lungimePlus :: Numar -> Int -> Numar
lungimePlus = undefined

-- 1b). Scrieti o functie care ia ca argument o pereche de numere
-- si calculeaza perechea de numere cu numar egal de cifre 
-- obtinuta prin adaugarea de zerouri la stanga numerelor date ca argument.
-- E.g., normalizeazaLungime ([1,2], [3,4,5,6]) = ([0,0,1,2], [3,4,5,6])
-- E.g., normalizeazaLungime ([1,2], []) = ([1,2], [0,0])
normalizeazaLungime :: (Numar, Numar) -> (Numar, Numar)
normalizeazaLungime = undefined


-- ==========================================

-- 2a) Scrieti o functie care ia doua Numere *de aceeasi lungime* ca argumente
-- si verifica daca primul Numar este mai mic sau egal cu al doilea.
-- Puteti folosi doar recursie si functii din categoria A
-- E.g., [1,2,3] `lteN` [1,2,1] = False
-- E.g., [0,2,3] `lteN` [1,2,1] = True
lteN :: Numar -> Numar -> Bool
lteN = undefined

-- 2b).  Scrieti o functie care ia doua Numere ca argumente
-- si verifica daca primul Numar este mai mic sau egal cu al doilea
lte :: Numar -> Numar -> Bool
lte  = undefined


-- ==========================================
-- Puteti rezolva exercitiile urmatoare prin orice metoda doriti, 
-- dar recomandam  rezolvarea folosind functiile foldl si foldr

-- 3a) Scrieti o functie care primeste ca argument un Numar 
-- si calculeaza valoarea de tip Integer care corespunde  Numar-ului respectiv
-- E.g., numar [1,0,4,0,0] = 10400

numar :: Numar -> Int
-- Sugestie: folositi foldl

-- foldr :: (a -b -b) -b -[a] -b
-- foldl :: (a -b -a) -a -[b] -a
numar xs = (foldl op unit xs) 1
    where
        unit = const 0
        (op r a) i = r (10 * i) + a * i
-- foldl (\a b -10*a + b) 0 [0,1,2,3,0]

-- 3b) Scrieti o functie care primeste ca argument o Cifra c si un Numar n
-- si calculeaza Numarul obtinut prin inmultirea lui n cu c.
-- Atentie: rezultatul trebuie sa fie lista de cifre (fara depasiri peste 10)
-- E.g., 4 `mulC` [1,0,4] = [4,1,6]
-- E.g., 9 `mulC` [9,9] = [8,9,1]
mulC :: Cifra -> Numar -> Numar
mulC = undefined


-- 3c) Scrieti o functie care primeste ca argument doua Numere
-- si calculeaza suma lor
-- Atentie: rezultatul trebuie sa fie lista de cifre (fara depasiri peste 10)
-- E.g., [7,2,3] `plus` [4,5,7] = [1,1,8,0]
-- E.g., [7,3] `plus` [4,5,7] = [5,3,0]
plus :: Numar -> Numar -> Numar
plus = undefined

