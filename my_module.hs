qsort list = 
	if null list 
	then []
	else 
		qsort [x | x <- tail list, x <= head list]
		++ [head list]
		++ qsort [x | x <- tail list, x > head list]

primes count =
	take count
	[x | x <- [2,3..], null [y | y <- [2..(div x 2)], mod x y == 0]]
