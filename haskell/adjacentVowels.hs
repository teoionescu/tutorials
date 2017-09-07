
{- http://csacademy.com/contest/round-47/task/adjacent-vowels/ -}

theNotNull :: Foldable t => t a -> Bool
theNotNull = not . null

isVow :: Char -> Bool
isVow ch = theNotNull (filter (\x -> x == ch) "aeiou")

cntVow [] = 0
cntVow (x : []) = 0
cntVow (x : y : xs)
    | (isVow x) && (isVow y) = 1 + (cntVow (y : xs))
    | otherwise = (cntVow (y : xs))

main = do
    val1 <- getLine
    val2 <- getLine
    print (cntVow val2)
