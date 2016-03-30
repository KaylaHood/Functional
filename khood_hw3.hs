--Kayla Hood, HW3, September 28th
--
--Problem 1
myfoldr :: (a -> b -> b) -> b -> [a] -> b
myfoldr f z [] = z
myfoldr f z (x:xs) = f x (myfoldr f z xs)

--Problem 2
myfoldl :: (b -> a -> b) -> b -> [a] -> b
myfoldl f z [] = z
myfoldl f z (x:xs) = myfoldl f (f z x) xs

--Problem 3 
concatenate :: [String] -> String
concatenate [] = ""
concatenate [x] = "x"
concatenate xs = myfoldr (++) "" xs

--Problem 4

build x [] = [x]
build x (y:ys) = if x <= y then x:y:ys else y:ys
--basically drops all elems that are out of order

sorted :: Ord a => [a] -> Bool
sorted [] = True
sorted [x] = True
sorted xs = let lst = myfoldr (build) [] xs
            in (length lst == length xs)

--Problem 5

range :: Ord a => [a] -> (a,a)
range [x] = (x,x)
range (x:y:xs) = let least = myfoldr (min) x (y:xs)
                     most = myfoldr (max) x (y:xs)
                 in (least,most)

--Problem 6

flatmap :: (t -> [a]) -> [t] -> [a]
flatmap f xs = myfoldr (\x acc -> (f x)++acc) [] xs

--Problem 7

exists :: (t -> Bool) -> [t] -> Bool
exists p [] = False
exists p (x:xs) = if (p x) then True else (exists p xs)

--Problem 8

forall :: (t -> Bool) -> [t] -> Bool
forall p [] = True
forall p (x:xs) = if (p x) then (forall p xs) else False 

--Problem 9

myPartition :: (a -> Bool) -> [a] -> ([a],[a])
myPartition p [] = ([],[])
myPartition p (x:xs) = ([c | c <- (x:xs), p c],[n | n <- (x:xs), not (p n)])
--list comprehensions ftw!

--Warm Up

foldyMap :: (a -> b) -> [a] -> [b]
foldyMap f xs = foldr (\x acc -> (f x):acc) [] xs 

--Problem 10

incByN :: (Num a) => [a] -> a -> [a]
incByN xs n = flatmap (\x -> [x+n]) xs

--Problem 11

sumPairs :: (Num a) => [a] -> [a] -> [a]
sumPairs [] [] = []
sumPairs (x:xs) (y:ys) = (y+x):(sumPairs xs ys)
