module Assignment2 where

--Main hopscotch function takes a list and returns a tuple
--calls on the bestpath and paths helper functions
hopscotch :: [Int] -> ([Int], Int)
hopscotch list
	| list == [] = error "Empty lists are bad"
	| otherwise =((bestpath (paths list) 0 []) ,sum (bestpath (paths list) 0 []))

--Helper function which takes a list and returns a list of lists
--base cases cover empty list, one element, and empty tails
--we recurse paths with a single and double jump and then map the first value onto the return value 
paths :: [Int] -> [[Int]]
paths [] = error "empty list"
paths [x] = [[x]]
paths (x:y:z:[]) = [[x,z]]
paths (x:y:[]) = [[x]]
paths(x:y:z:ys) = map(x:) (paths (z:ys) ++ paths (ys))

--Helper function which takes a list of lists, a value to store the largest sum and a list to store the list with the largest sum
--Base cases cover empty list of lists and empty tail of the list of lists
--compares the sum of the first list and the head of the tail and recurses depending on which returns a greater value
bestpath :: [[Int]] -> Int -> [Int] -> [Int]
bestpath (x:xs) max list
	| (x:xs) == [] = list
	| (xs) == [] = list
 	| sum(x) >= (sum (head (xs))) = bestpath xs (sum(x)) x
	| otherwise = bestpath (xs) (sum(head(xs))) (head (xs))



countName (MakeTree name []) n
	| name /= n = 0
	| otherwise = 1
countName (MakeTree name kids) n
	| name == n = 1+(countKids kids n)
	| otherwise = countKids kids n
countKids [] n = 0
countKids ((MakeTree name kids):trees) 
	n = (countName (MakeTree name kids) n) + (countKids trees n)