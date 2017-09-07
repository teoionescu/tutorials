
{- http://csacademy.com/contest/round-47/task/expected-merge/ -}

div2 x = quot x 2
div2plus1 x = div2 x + 1
getMod f n s = getValue (f n) (mod s (f n))

getValue n s
    | n == 1                        = 1.0
    | mod n 2 == 0                  = fromIntegral n + (getMod div2 n s)
    | mod n 2 == 1 && s == (div2 n) = fromIntegral n + (getMod div2plus1 n s)
    | mod n 2 == 1 && s  > (div2 n) = getValue n (n-s-1)
    | otherwise                     = fromIntegral n + ((getMod div2 n s) + (getMod div2plus1 n s))/2

memo f = map (\x -> map (f x) [0..]) [0..]
getValueStore = memo getValue
fastGetValue x y = getValueStore !! x !! y
    
solve n = [ fastGetValue n i | i <- [0..(n-1)]]

cycleSpaces = [ s | s <- " " : cycleSpaces ]
format :: Show a => [a] -> String
format list = (init . concat) ( zipWith (++) (map show list) cycleSpaces )

main = do
    val1 <- getLine
    let answer = solve (read val1)
    putStrLn $ id (format answer)
