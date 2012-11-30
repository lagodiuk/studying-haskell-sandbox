qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (h:t) = 
	qsort [x | x <- t, x <= h]
	++ [h]
	++ qsort [x | x <- t, x > h]

primes :: Int -> [Integer]
primes count =
	take count
	[x | x <- [2,3..], null [y | y <- [2..(div x 2)], mod x y == 0]]

pairs :: [a] -> [(a,a)]
pairs list = pairs' list []

pairs' :: [a] -> [(a,a)] -> [(a,a)]
pairs' [] acc = reverse acc
pairs' (_:[]) acc = pairs' [] acc
pairs' (x:y:tl) acc = pairs' (y:tl) ((x, y):acc)

fib_list :: Integer -> [Integer]
fib_list n = fib_list' n []

fib_list' :: Integer -> [Integer] -> [Integer]
fib_list' 0 list = reverse list
fib_list' n [] = fib_list' (n-1) (1:[])
fib_list' n (1:[]) = fib_list' (n-1) (1:1:[])
fib_list' n (a:b:tl) = fib_list' (n-1) ((a+b):a:b:tl)
