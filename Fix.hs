{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

data Fix f = In (f (Fix f))
deriving instance (Show (f (Fix f))) => Show (Fix f)

inop :: Fix f -> f (Fix f)
inop (In x) = x

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . inop

-- Circuits --
data Circuit' k = Identity' Int
                 | Above' k k
                 | Beside' k k
                 | Fan' Int
                 | Stretch' [Int] k deriving Show

instance Functor Circuit' where
    fmap f (Identity' w)    = Identity' w
    fmap f (Above' x y)     = Above' (f x) (f y)
    fmap f (Beside' x y)    = Beside' (f x) (f y)
    fmap f (Fan' w)         = Fan' w
    fmap f (Stretch' ws k)  = Stretch' ws (f k)

width :: Fix Circuit' -> Int
width = cata width' where
    width' :: Circuit' Int -> Int
    width' (Identity' w)    = w
    width' (Above' x y)     = x
    width' (Beside' x y)    = x + y
    width' (Fan' w)         = w
    width' (Stretch' ws k)  = sum ws

-- Expressions --
data Expr' k = Var' Int
             | Plus' k k
             | Minus' k k

instance Functor Expr' where
    fmap f (Var' w) = Var' w
    fmap f (Plus' x y) = Plus' (f x) (f y)
    fmap f (Minus' x y) = Minus' (f x) (f y)
