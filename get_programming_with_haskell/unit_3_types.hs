type Document  = String

-- Breakfast specials
data BreakfastSide = Toast | Biscuit | Homefries | Fruit deriving (Show)
data BreakfastMeat = Sausage | Bacon | Ham deriving (Show)
data BreakfastMain = Egg | Pancake | Waffle deriving (Show)


-- PRODUCT TYPES


-- C struct example as a prototype for product types in other languages
-- that is Classes and JSON

-- struct author_name {
--  char *first_name;
--  char *last_name;
-- };

-- struct book {
--   author_name author;
--   char *isbn;
--   char *title;
--   int  year_published;
--   double price;
-- };


-- Listing 16.2. C’s author_name and book structs translated to Haskell
-- data AuthorName = AuthorName String String

-- data Book = Author String String Int

-- AuthorName using record syntax
data AuthorName = AuthorName {
    firstName :: String,
    lastName :: String
}

-- Listing 16.3. Using record syntax for Book to show the similarity to a C struct
data Book = Book {
     author  :: AuthorName
   , isbn    :: String
   , title   :: String
   , year    :: Int
   , price   :: Double}

-- ATTENTION
-- What’s fascinating is that in most programming languages, combining types with an and is the only way to make new types.

data VinylRecord = VinylRecord {
     artist        :: Creator
   , recordTitle   :: String
   , recordYear    :: Int
   , recordPrice   :: Double
   }