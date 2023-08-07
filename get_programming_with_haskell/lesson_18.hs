import qualified Data.Map as Map

data Box a = Box a deriving Show

wrap :: a -> Box a
wrap x = Box x

unwrap :: Box a -> a
unwrap (Box x) = x


itemCount1 :: (String,Int)
itemCount1 = ("Erasers",25)

itemCount2 :: (String,Int)
itemCount2 = ("Pencils",25)

itemCount3 :: (String,Int)
itemCount3 = ("Pens",13)


data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq)

organs :: [Organ]
organs = [Heart,Heart,Brain,Spleen,Spleen,Kidney]

ids :: [Int]
ids = [2,7,13,14,21,24]

organPairs :: [(Int, Organ)]
organPairs = zip ids organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs