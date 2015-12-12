module Assignment1 where 

--Question 1
--main isPerfect function
--Calls perfectHelper to check if a number is a perfect number
isPerfect :: Int -> Bool
isPerfect num
	| perfectHelper num num == 2 * num = True
	| otherwise = False
--Helper Function
perfectHelper :: Int -> Int -> Int
-- mods x with y to see if it is a factor of x
-- if it is a factor we recursively call perfectHelper and add it to y
perfectHelper x y
 | y == 0 = 0
 | mod x y == 0 = y + perfectHelper x (y - 1)
 | otherwise = perfectHelper x (y-1)

--Question 2
--Calls another function and returns the nth value that is true for the function
nthTrue ::(Int -> Bool) -> Int -> Int
nthTrue func num	
	| num < 1 = error "error, please provide a number greater than 0"
	| otherwise = nthHelper 0 func num 0

--Helper Function
--takes a value a function the upper limit and a count value
--value is used on the function and each call of nthHelper increments the/
--value by 1. Each time the function return True count is incremented by 1
--when count reaches the upper limit we return the value.
nthHelper :: Int -> (Int -> Bool) -> Int -> Int -> Int
nthHelper val fun lim count
	| count == lim + 1 = val -1
	| fun val == True = nthHelper (val + 1) fun lim (count + 1)
	| otherwise = nthHelper (val + 1) fun lim count
