import Data.List
import System.IO

maxInt = maxBound :: Int

modEx = 5 `mod` 4
primeNumbers = [2,3,5,7,11]
more = primeNumbers ++ [13,17]
fav = 2 : 3 : 7 : []
isFavEmpty = null fav
lengthFav = length fav

secondFav = fav !! 1

isSevenIn = 7 `elem` fav
timesTwo = [x * 2 | x <- [1..1000], x * 3 <= 100]
sumOfLists = zipWith (+) [1,2,3,4,5] [6,7,8,9,10]
evensUpTo = takeWhile (<= 20) [2,4..]
multOfList = foldl (*) 1 [2,3,4,5]

{- Tuple -}
randTuple = (1,"Ima Tuple")

doubleList nums =
    if null nums
    then []
    else (2 * (head nums)) : (doubleList (tail nums))
req = doubleList primeNumbers
{- head and tail are fragile functions -}

pow2 n
    | n == 0    = 1
    | otherwise = 2 * (pow2 (n-1))

removeOdd [] = []
removeOdd (x : xs)
    | mod x 2 == 0  =  x : (removeOdd xs)
    | otherwise     =  removeOdd xs

anyEven nums = case (removeOdd nums) of
    []          ->  False
    (x : xs)    ->  True

howManyEven nums =
    let evenNums = removeOdd nums
    in length evenNums

always7 = const 7 {- function that always returns 7 -}

doubleDaList = map (2*)

notNull xs = not (null xs)
removeNulls = filter notNull ["","1234","","abcd"]
removeOddBetter = filter (\ x -> x `mod` 2 == 0)

numberLength x = length (show x)

theNotNull :: Foldable t => t a -> Bool
theNotNull = not . null

omg = zipWith ($) [(+1), (\x -> 2 * x + 1), (*2)] [1,2,3]


myHead :: [a] -> a
myHead (x : xs) = x


data StringTree = StringTree String [StringTree]
tree = StringTree "b"
        [ StringTree "a" []
        , StringTree "u"
            [
                StringTree "n" []
            ]
        , StringTree "e" []
        ]

data MaybeInt = NoInt | JustInt Int
defaultInt :: Int -> MaybeInt -> Int
defaultInt defaultValue NoInt = defaultValue
defaultInt _ (JustInt x) = x

{- Type Synonyms and NewType -}
type Point = (Double, Double)

newtype Customer = Customer Int
extract :: Customer -> Int
extract (Customer i) = i
{- data: newtype with more args -}

{- Type Class Instances -}
isElemInList :: Eq a => a -> [a] -> Bool
isElemInList _ [] = False
isElemInList x (y : ys)
    | x == y     =  True
    | otherwise  =  isElemInList x ys

data RGB = RGB Int Int Int
colorList = [RGB 255 0 0, RGB 0 255 0, RGB 0 0 255]
green = RGB 0 255 0
greenInColorList = isElemInList green colorList

instance Eq RGB where
    (RGB r1 g1 b1) == (RGB r2 g2 b2) =
        (r1 == r2) && (g1 == g2) && (b1 == b2)

instance Show RGB where
    show (RGB r g b) =
        "RGB " ++ (show r) ++ " " ++ (show g) ++ " " ++ (show b)

{- Type Class Instances For Parameterized Types -}
data Maybe' a = Nothing' | Just' a
instance (Eq a) => Eq (Maybe' a) where
    Nothing' == Nothing' = True
    Nothing' == (Just' _) = False
    (Just' _) == Nothing' = False
    (Just' x) == (Just' y) = x == y

maybeExposerOfNum :: Num a => Maybe' a -> [Char]
maybeExposerOfNum Nothing' = "Nuthin"
maybeExposerOfNum (Just' _) = "Sumthin"

{- Defining Type Classes -}
data Point2 = Point2 Double Double
    deriving Show
data Point3 = Point3 Double Double Double
    deriving Show
class MeasurableDistance a where
    distance :: a -> a -> Double
    repeater :: a -> a

instance MeasurableDistance Point2 where
    distance (Point2 x1 y1) (Point2 x2 y2) =
        sqrt(dx * dx + dy * dy)
        where dx = x1 - x2
              dy = y1 - y2
    repeater (Point2 x y) = (Point2 x y)

pathLength :: MeasurableDistance a => [a] -> Double
pathLength [] = 0
pathLength (_ : []) = 0
pathLength (p0 : p1 : ps) = distance p0 p1 + pathLength (p1 : ps)

{- Mona-Do -}
first = [1,2,3]
second = [1,2,3]
pairs = do
    x <- first
    y <- second
    return ( x*10 + y )

{- Main -}
main :: IO ()
main = do
    -- line <- getLine
    -- putStrLn ("Hello " ++ line)

    line1 <- getLine
    line2 <- getLine
    {- lines <- return (line1 ++ line2) -}
    let lines = line1 ++ line2
    putStrLn lines