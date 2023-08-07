-- Euclid's algorithm
-- Find GCD (greated common divisor) for two numbers
{-
1. You start with two numbers, a and b.
2. If you divide a by b and the remainder is 0, clearly b is the GCD.
3. Otherwise, you change the value of a by assigning it the value of b (b becomes the new a). You also change the value of b to be the remainder you obtained in step 2 (the new b is the remainder of the original a divided by the original b).
4. Then repeat until a/b has no remainder.
-}

{-
Let’s work through one example:

1. a = 20, b = 16
2. a/b = 20/16 = 1 remainder 4
3. a = 16, b = 4
4. a/b = 4 remainder 0
5. GCD = b = 4
-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

myGCD a b = if remainder == 0
    then b
    else myGCD b remainder
    where remainder = mod a b

-- Pattern Matching
sayAmount n = case n of
    1 -> "one"
    2 -> "two"
    n -> "a bunch"

myHead n = case n of 
    (x:xs) -> x
    [] -> error "No head for empty list"

myTail n = case n of
    (_:xs) -> xs
    _ -> []

myGCDPatternM :: Integral a => a -> a -> a
myGCDPatternM a b = case remainder of
    0 -> b
    _ -> myGCD b remainder
    where remainder = mod a b

myDrop a _list = _list!!a