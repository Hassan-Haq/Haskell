module Recursion where
--calculate the factorial
factorial 0 = 1
factorial n | n < 0 = error "factorial of neg not defined"
factorial n = n * factorial (n-1)
--ask if an interger is a positive power of 2
pow2 :: Int -> Bool
pow2 1 = True
pow2 n 
	| mod n 2 == 0 = pow2 (div n 2)
	| otherwise = False
--sums all the functions given to it
sumfunc :: (Int->Int) -> Int -> Int -> Int
sumfunc f low high
	| low > high = 0
	| otherwise = (f low) + (sumfunc f (low +1) high)
--ask if an integer is prime
--helper function to see if a value has a factor 
hasFactor n low high
	| low > high = False
	| mod n low == 0 = True
	| otherwise = hasFactor n (low+1) high

isPrime n 
	| n < 1 = False
isPrime n = not (hasFactor n 2 (div n 2))
--Calculate the number of primes less than or equal to the number
primesLeq n
	| n < 2 = 0
	| isPrime n = 1 + primesLeq (n-1)
	| otherwise = primesLeq (n-1)
	
firstPrime Geq n 
	| isPrime n = n
	| otherwise = firstPrime Geq (n+1)
	
--find the first prime >=N
firstPrime n
	| isPrime n = n 
	| otherwise = firstPrime (n+1)
--find the smallest prime factor of an integer
lowPrimeInRange low high
	| low > high = error "no prime numbers in the range"
	| isPrime low = low
	|otherwise = lowPrimeInRange (low+1) high
	
smallestFactorInRange n low high
	| n <= 1  = error "noprime factor"
	| mod n low == 0 = low
	| low > high = n
	|otherwise = smallestFactorInRange n 2 sqrtFloor
	where 
		sqrtFloor = floor (sqrt (fromIntegral n))

		
smallestFactor n = smallestFactorInRange n 2 n

smallestPrimeFactor n
	| n <= 1 = error "no prime factor"
	| otherwise = not (hasFactorInRange n 2 sqrtFloor)
	where
	--sqrtFloor is the square root of n rounded down to an integer
	sqrtFloor = floor (sqrt (fromIntegral n))
	
distinctPrimeFactors n
	| n <=1 = error "xxx"
	| isPrime n = 1
	| otherwise = 1 + numPrimeFactors (div n firstFactor)
	where
	firstFactor = smallestPrimeFactor n
divMany n f
	|mod n f /= 0 = n
	|otherwise = divMany (div n f) f
numPrimeFactors n
	| n <=1 = 0
	| isPrime n = 1
	--n > = 2 and not prime
	| otherwise = numPrimeFactors (div n minFactor) + 1
	where
		minFactor = smallestFactor n 