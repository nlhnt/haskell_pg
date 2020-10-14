lostNumbers = [1,2,3,4,5,6,7]

-- get even natural numbers which are not less than 12 and not greater than 20
naturalNumbers = [x*2 | x <- [1..10], x*2 >= 12]

-- comment if number is less than 10 we give replace it with "BOOM!"
    -- else we replace it with a "BANG!"
    -- only if number is less than 10
boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]