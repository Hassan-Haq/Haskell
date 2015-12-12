

foldPairs _ [] = []
foldPairs func (x:xs)
	|(length(x:xs)) `mod` 2 /= 0 = error "The list is odd length"
	|otherwise = func x (head xs): foldPairs func (tail xs)
	

--compFuncs fs x = (foldr1 . f) x

reverse1 xs = foldl comb [] xs
	where comb list x = x : list
	
reverse2 xs = foldr snoc [] xs
	where	
	snoc x xs = xs ++ [x]
