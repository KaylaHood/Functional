--pledged Kayla Hood, August 29th 2015

import Data.List

collatzEsque x = if x >= 0 then (-1 * (x - 10)) else (-2 * x)

firstTwo x = take 2 x

firstTwoTup x = (x!!0,x!!1)

evenUpTo x = [0,2..x]

isDivisible a b = if b `mod` a == 0 then 1 else 0

divisiblePairs x = [(m,n) | m <- x, n <- x, m /= n, isDivisible m n == 1]

findStringsOfLength len strs = [x | x <- strs, length x == len]

stringsOfLengths lens strs =  [ (x, findStringsOfLength x strs) | x <- lens ]

primes = [x | x <- [1..], x == 2 || x `mod` 2 /= 0, x == 3 || x `mod` 3 /= 0, x == 5 || x `mod` 5 /= 0, x == 7 || x `mod` 7 /= 0]

--this next one might be a bit cheaty but, as you asked in the problem, I did use list comprehension to do it.

powerset x = [y | y <- subsequences x] 

--I tried doing it a different way, but I kept getting errors and StackOverflow has an answer but it looks like you need to use more advanced concepts

--(they said to use "continuousSubSeqs = filter (not . null) . concatMap inits . tails" or 

--"continuousSubSeqs ls = [t | i <- inits ls, t <- tails i, not $ null t]" to get a powerset without the empty list in it, or to use

--"filterM (const [True,False]) [1,2,3]" to get a list with the empty set in it. I don't understand how the "." notation in Haskell works and 

--the last one looks like it was written in C++, not Haskell (but StackOverflow says it is okay???) 
