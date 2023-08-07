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

data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6

instance Show SixSidedDie where
    show S1 = "I"
    show S2 = "II"
    show S3 = "III"
    show S4 = "IV"
    show S5 = "V"
    show S6 = "VI"

instance Eq SixSidedDie where
    (==) S6 S6 = True
    (==) S5 S5 = True
    (==) S4 S4 = True
    (==) S3 S3 = True
    (==) S2 S2 = True
    (==) S1 S1 = True
    (==) _ _ = True

instance Ord SixSidedDie where
    compare S6 S6 = EQ
    compare S6 _ = GT
    compare _ S6 = LT
    compare S5 S5 = EQ
    compare S5 _ = GT
    compare _ S5 = LT
    compare S4 S4 = EQ
    compare S4 _ = GT
    compare _ S4 = LT
    compare S3 S3 = EQ
    compare S3 _ = GT
    compare _ S3 = LT
    compare S2 S2 = EQ
    compare S2 _ = GT
    compare _ S2 = LT
    compare S1 S1 = EQ

-- 14.7. Type classes for more-complex types
-- type Name = (String, String)
-- we need a new data type to override compare for tuples of Strings
-- data Name = Name (String, String) deriving (Show, Eq)
-- this is a special case for using newtype keyword
newtype Name = Name (String, String) deriving (Show, Eq)
names :: [Name]
names = [
    Name ("Emil","Cioran")
    , Name ("Eugene","Thacker")
    , Name ("Friedrich","Nietzsche")]

instance Ord Name where
    compare (Name (f1, l1)) (Name (f2, l2)) = compare (l1, f1) (l2, f2)