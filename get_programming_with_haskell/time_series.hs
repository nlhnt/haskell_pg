import Data.List
import qualified Data.Map as Map
import Data.Semigroup
import Data.Maybe

file1 :: [(Int,Double)]
file1 = [ (1, 200.1), (2,  199.5), (3, 199.4)
        , (4, 198.9), (5,  199.0), (6, 200.2)
        , (9, 200.3), (10, 201.2), (12, 202.9)]

file2 :: [(Int,Double)]
file2 = [(11, 201.6), (12, 201.5), (13, 201.5)
        ,(14, 203.5), (15, 204.9), (16, 207.1)
        ,(18, 210.5), (20, 208.8)]

file3 :: [(Int,Double)]
file3 = [(10, 201.2), (11, 201.6), (12, 201.5)
        ,(13, 201.5), (14, 203.5), (17, 210.5)
        ,(24, 215.1), (25, 218.7)]

file4 :: [(Int,Double)]
file4 = [(26, 219.8), (27, 220.5), (28, 223.8)
        ,(29, 222.8), (30, 223.8), (31, 221.7)
        ,(32, 222.3), (33, 220.8), (34, 219.4)
        ,(35, 220.1), (36, 220.6)]


data TS a = TS [Int] [Maybe a]
createTS :: [Int] -> [a] -> TS a
createTS times values = TS completeTimes extendedValues              -- 1
   where completeTimes = [minimum times .. maximum times]            -- 2
         timeValueMap = Map.fromList (zip times values)              -- 3
         extendedValues = map (\v -> Map.lookup v timeValueMap)      -- 4
                             completeTimes
-- 1 You want to create your time series with a full timeline and a list of Maybe values; you assume the arguments may represent only a limited set of possible values.
-- 2 The completeTimes are all the times from the minimum passed into the function up to the maximum.
-- 3 You’ll create a simple Map of the times and values you know you have.
-- 4 By mapping lookup over complete times, you’ll get Just x values for all existing values and Nothing for all missing values. This takes care of filling in your complete set of values to match the complete timeline (even if some of those values are missing).

fileToTS :: [(Int, a)] -> TS a
fileToTS tvPairs = createTS times values where
    (times, values) = unzip tvPairs

showTVPair :: Show a => Int -> (Maybe a) -> String
showTVPair time (Just value) = mconcat [show time, "|", show value, "\n"]
showTVPair time Nothing = mconcat [show time, "|NA\n"]

instance Show a => Show (TS a) where
    show (TS times values) = mconcat rows where
        rows = zipWith showTVPair times values

ts1 :: TS Double
ts1 = fileToTS file1

ts2 :: TS Double
ts2 = fileToTS file2

ts3 :: TS Double
ts3 = fileToTS file3

ts4 :: TS Double
ts4 = fileToTS file4

-- Now we need Semigroup & Monoid to mconcat these TimeSeries

insertMaybePair :: Ord k => Map.Map k v -> (k, Maybe v) -> Map.Map k v
insertMaybePair myMap (_,Nothing) = myMap                              --  1
insertMaybePair myMap (key,(Just value)) = Map.insert key value myMap  --  2

-- 1 Because your map is of actual values, you can ignore the case when the Maybe value is missing by returning the original Map.
-- 2 If you have an actual value, you grab it out of the Just context and insert it into your map.

combineTS :: TS a -> TS a -> TS a
combineTS (TS [] []) ts2 = ts2                                         --  1
combineTS ts1 (TS [] []) = ts1                                         --  1
combineTS (TS t1 v1) (TS t2 v2) = TS completeTimes combinedValues
  where
    bothTimes = mconcat [t1, t2]                                       --  2
    completeTimes = [minimum bothTimes .. maximum bothTimes]           --  3
    tvMap = foldl insertMaybePair Map.empty (zip t1 v1)                --  4
    updatedMap = foldl insertMaybePair tvMap (zip t2 v2)               --  5
    combinedValues = map (\v -> Map.lookup v updatedMap) completeTimes --  6

-- 1 If one series is empty, return the nonempty one.
-- 2 This indicates all the times in your two TS types. Duplicates may happen, but you use only the minimum and maximum values from the combined times.
-- 3 Now you can make a complete timeline for both TS types.
-- 4 You first insert all the values from ts1 into your Map. The zip function creates a list of time/value pairs, and you use foldl to insert them into the Map.
-- 5 Then you update that Map with the values from ts2. Inserting this way means duplicate values will automatically be overwritten by the values from ts2.
-- 6 Finally, you create your list of Maybe values by looking up all the completed times.

instance Semigroup (TS a) where
    (<>) = combineTS

instance Monoid (TS a) where
    mempty = TS [] []
    mappend = (<>)

tsAll :: TS Double
tsAll = mconcat [ts1, ts2, ts3, ts4]


-- Now that we have timeseries we can finally perform some calculations
-- We cleared the duplicates (assuming newer files were more important - their data is more up to date)
-- We have Semigroup and Monoid
-- Time to do some "work"
-- That is, let haskell do that work for us haha
-- Let's create a summary statistic, similar to pandas DataFrame.describe in python world

-- Let's start with the average values in our timeseries
mean :: (Real a) => [a] -> Double
mean xs = total/count where
    total = (realToFrac . sum) xs
    count = (realToFrac . length) xs

meanTS :: (Real a) => TS a -> Maybe Double
meanTS (TS _ []) = Nothing
meanTS (TS times values) = if all (== Nothing) values
    then Nothing
    else Just avg where
        justVals = filter isJust values      --  1
        cleanVals = map fromJust justVals    --  2
        avg = mean cleanVals

-- 1 isJust requires the import of the Data.Maybe module, and tests whether a value “is Just.”
-- 2 fromJust, also in Data.Maybe, is the equivalent of (\(Just x) -> x)

type CompareFunc a = a -> a -> a
type TSCompareFunc a = (Int, Maybe a) -> (Int, Maybe a) -> (Int, Maybe a)

makeTSCompare :: Eq a => CompareFunc a -> TSCompareFunc a
makeTSCompare func = newFunc                                          --  1
 where newFunc (i1, Nothing) (i2, Nothing) = (i1, Nothing)            --  2
       newFunc (_, Nothing) (i, val) = (i,val)                        --  3
       newFunc (i, val) (_, Nothing) = (i,val)                        --  3
       newFunc (i1,Just val1) (i2,Just  val2) =                       --  4
                                      if func val1 val2 == val1
                                      then  (i1, Just val1)
                                      else  (i2, Just val2)

-- 1 Here you’re creating a newFunction to return.
-- 2 Even though you’re in a where, you can still do pattern matching.
-- 3 These first three cases handle when one or both values are Nothing.
-- 4 This last definition performs the behavior of the comparison function, only it returns the full tuple.