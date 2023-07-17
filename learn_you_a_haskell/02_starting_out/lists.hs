lostNumbers = [1,2,3,4,5,6,7]

-- get even natural numbers which are not less than 12 and not greater than 20
naturalNumbers = [x*2 | x <- [1..10], x*2 >= 12]

-- comment if number is less than 10 we give replace it with "BOOM!"
    -- else we replace it with a "BANG!"
    -- only if number is less than 10
boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

-- several predicates
fewPredicatesFunct = [ x | x <- [10..20], x /= 13, x /= 15, x /= 19]

-- get products of all available combinations of products of x and y elements
allCombinations = [ x*y | x <- [2,5,10], y <- [8,10,11]]

-- as above, but products must evaluate to be more than 50
allMoreThan50 = [ x*y | x <- [2,5,10], y <- [8,10,11], x*y > 50]

-- finished at adjectives and nouns lists