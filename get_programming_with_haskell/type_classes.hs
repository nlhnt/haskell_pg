aList = ["cat","dog","mouse"]

-- Listing 13.1. Num type class definition
-- GHCi> :info Num
-- class Num a where
--    (+) :: a -> a -> a
--    (-) :: a -> a -> a
--    (*) :: a -> a -> a
--    negate :: a -> a
--    abs :: a -> a
--    signum :: a -> a

class Describable a where
    describe :: a -> String

-- derive Show

data Icecream = Chocolate | Vanilla deriving (Show, Eq, Ord)
-- ghci> Chocolate
-- Chocolate
-- ghci> Vanilla
-- Vanilla
-- Vanilla > Chocolate because:
-- If you add deriving Ord to your definition of Icecream, 
-- Haskell defaults to the order of the data constructors for determining Ord. 
-- So Vanilla will be greater than Chocolate.