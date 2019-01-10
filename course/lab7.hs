
import Data.Functor
import Data.List.Split
import Data.List

showPlusZece :: Integer -> IO Integer
showPlusZece n = putStrLn (show n) $> n + 10
showPlusZece2 :: Integer -> IO Integer
showPlusZece2 n = showPlusZece n >>= showPlusZece

smen = fmap read getLine >>= showPlusZece


t :: Integer -> IO Integer
t x = return x
r = fmap read getLine >>= showPlusZece >>= t >>= t

cit = do
    x <- fmap read getLine
    putStrLn (show x)
    let s = x + 10
    return s

-- LAB
oNumber = do
    noin <- readLn :: IO Double
    putStrLn $ "Intrare\n" ++ (show noin)
    let  noout = sqrt noin
    putStrLn $ "Iesire"
    print noout

inoutFile = do
    sin <- readFile "Input.txt"
    putStrLn $ "Intrare\n" ++ sin
    let sout = sin ++ sin
    putStrLn $ "Iesire\n" ++ sout
    writeFile "Output.txt" sout

f = do
    sin <- readFile "Input.txt"
    let l = lines sin
    let n = read (l !! 0) :: Int
    let t = map (splitOn ",") $ take n (tail l)
    let max = maximum $ map (\x -> read (x !! 1) :: Int) t
    let high = filter (\x -> (read (x !! 1) :: Int) == max) t
    let names = map (!! 0) high
    let act = map (putStrLn . show) names
    sequence_ act
    putStr $ concat $ intersperse "\n" $ map (\x -> [x]) (['a'..'e'])
-- unwords $ map (\x -> [x]) ['a'..'z']


