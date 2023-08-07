computeChange owed given =
  if given - owed > 0
    then given - owed
    else 0

-- Why does max work in this example?
-- Because it is a function that accepts two args
-- 1st (given - owed)
-- 2nd 0
-- Returns max between these two, which is the same as above
computeChangeMax owed given = max (given - owed) 0

computeChangeWhere owed given =
  if change > 0
    then change
    else 0
  where
    change = given - owed

computeChangeWhereMax owed given = max change 0
  where
    change = given - owed

doublePlusTwo x = doubleX + 2
  where
    doubleX = x * 2

inc n = n + 1

double n = n * 2

square n = n ^ 2

squareFloat n = n ** 2

evenOddFun n =
  if even n
    then n - 2
    else 3 * n + 1

sumSquareOrSquareSum x y =
  if sumSquare > squareSum
    then sumSquare
    else squareSum
  where
    sumSquare = x ^ 2 + x ^ 2
    squareSum = (x + y) ^ 2

sumSquareOrSquareSumLambda x y =
  ( \sumSquare squareSum ->
      if sumSquare > squareSum
        then sumSquare
        else squareSum
  )
    (x ^ 2 + y ^ 2)
    ((x + y) ^ 2)

x = 4

add1 y = y + x

add2 y = (\x -> y + x) 3

add3 y =
  ( \y ->
      (\x -> y + x) 1
  )
    2

counter x =
  let x = x + 1
   in let x = x + 1
       in x

counterLambda x = (\x -> x + 1)
  ((\x -> x + 1) ((\x -> x) x))

counterLambdaInfix x = (+ 1)
  ((+ 1) x)

ifEvenInc n =
  if even n
    then n + 1
    else n

-- ifEven :: Integral t => (t -> t) -> t -> t
ifEven myFunction x = if even x
    then myFunction x
    else x

ifEvenDouble :: Integer -> Integer
-- ifEvenDouble = ifEven (\x -> x*2)
-- using infix
ifEvenDouble = ifEven (*2)

sfOffice :: ([Char], [Char]) -> [Char]
sfOffice name = if lastName < "L"
  then nameText ++ " - PO Box 1234 - San francisco, CA, 94111"
  else nameText ++ " - PO Box 1010 - San Francisco, CA, 94109"
  where lastName = snd name
        nameText = (fst name) ++ " " ++ lastName


ifEvenFactory f = \x -> ifEven f x