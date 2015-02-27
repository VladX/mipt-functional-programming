fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib nn = if even nn
	then let n = div nn 2 in
		(2 * fib (n - 1) + fib n) * fib n
	else let n = div (nn+1) 2 in
		fib n ^ 2 + fib (n-1) ^ 2

main = putStrLn (show (fib 5000))