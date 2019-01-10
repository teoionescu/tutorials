
-- Identity Monad

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
