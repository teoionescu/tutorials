

-- You Could Have Invented Monads (And Maybe You Already Have.) by Dan Piponi

module Logger where
    import Data.Char
    import Test.QuickCheck
    
    type Logger a = (a,String)
    
    -- Given a function, debug produces a function logging information 
    -- about the computed value for each argument
    debug :: (Show a, Show b) =>  (a -> b) -> (a -> Logger b)
    debug f x = (y, show x ++ " |-> " ++ show y ++ "; ")
      where
        y = f x
    
    ord' :: Char -> Logger Int
    ord' = debug ord
    
    chr' :: Int -> Logger Char
    chr' = debug chr
    
    -- Exercise 1
    bind :: (a -> Logger b) -> (Logger a -> Logger b)
    bind f a = let (y, z) = f (fst a) in (y, snd a ++ z)
    
    -- Using # instead of * for composition to avoid ambiguities
    (#) :: (b -> Logger c) -> (a -> Logger b) -> (a -> Logger c)
    g' # f' = bind g' . f'
    
    -- Exercise 2
    unit :: a -> Logger a
    unit a = (a, "")
    
    -- lift --- lifting functions
    lift :: (a -> b) -> (a -> Logger b)
    lift f = unit . f
    
    -- Test that (for a given value x) lift g # lift f = lift (g.f)
    -- For simplicity we restrict to Float functions as in the tutorial
    check_lift :: (Float -> Float) -> (Float -> Float) -> Float -> Bool
    check_lift f g x = ((lift g) # (lift f)) x == lift (g.f) x
    
    test_lift :: IO ()
    test_lift = quickCheck $ check_lift (+2) (*3)
    
    -- Exercise Ten (a): Rewrite the module to make Logger instance of 
    -- the Monad typeclass
    -- Note: You first need to make it an instance of Functor and Applicative