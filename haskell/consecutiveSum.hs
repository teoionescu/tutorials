
{- http://csacademy.com/contest/round-47/task/consecutive-sum/ -}

sumOf :: Int -> Int
sumOf s = quot (s*(s+1)) 2

leftOver n s = (quot (n-(sumOf s))) s

okCheck n s =
    if mod (n-(sumOf s)) s == 0
    then map ((leftOver n s)+) [1, s]
    else []

testOf n s
    | (sumOf s) <= n  =  okCheck n s
    | otherwise       =  [-1]

findSum n s = case (testOf n s) of
    []          ->  findSum n (s+1)
    (x : xs)    ->  (x : xs)

cycleSpaces = [ s | s <- " " : cycleSpaces ]

format :: Show a => [a] -> String
format list = (init . concat) ( zipWith (++) (map show list) cycleSpaces )

main = do
    val1 <- getLine
    let answer = (findSum (read val1) 2)
    putStrLn $ id (format answer)
