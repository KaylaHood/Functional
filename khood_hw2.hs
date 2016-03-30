--Problem 1

concatenate :: [String] -> String
concatenate (x:xs) = x ++ (concatenate xs)
concatenate [] = ""

myMinimum :: Ord a => [a] -> a
myMinimum (x:[]) = x
myMinimum (x:xs) = min x (myMinimum xs)

myMaximum :: Ord a => [a] -> a
myMaximum (x:[]) = x
myMaximum (x:xs) = max x (myMaximum xs)

--Problem 2

range :: Ord a => [a] -> (a,a)
range (x:[]) = (x,x)
range (x:xs) = (min x (myMinimum xs), max x (myMaximum xs))

myElem :: Eq a => a -> [a] -> Bool
myElem elem [] = False
myElem elem [x] = elem == x
myElem elem (x:xs) = elem == x || myElem elem xs

quickSort [] = []
quickSort [x] = [x]
quickSort (x:xs) = 
    let ltX = [y | y <- xs, y <= x]
        gtX = [y | y <- xs, y > x]
        sorted_ltX = quickSort ltX
        sorted_gtX = quickSort gtX
    in sorted_ltX ++ [x] ++  sorted_gtX

--Problem 3
--
rev :: [a] -> [a]
rev [] = []
rev (x:[]) = [x]
rev (x:xs) = (rev xs) ++ [x]

--Problem 4

sorted :: Ord a => [a] -> Bool
sorted [] = True
sorted [x] = True
sorted (x:y:xs) = (x <= y) && sorted xs

--Problem 5

myTake :: Int -> [a] -> [a]
myTake n _ 
    | n <= 0 = []
myTake _ [] = []
myTake n (x:xs) = x : myTake (n-1) xs 

myDrop :: Int -> [a] -> [a]
myDrop n xs 
    | n <= 0 = xs
myDrop _ [] = []
myDrop n (_:xs) = myDrop (n-1) xs

dropEveryNth :: Int -> [a] -> [a]
dropEveryNth _ [] = []
dropEveryNth n xs 
    | n <= 0 = xs
dropEveryNth n xs = (myTake (n-1) xs) ++ (dropEveryNth n (myDrop n xs))

--Problem 6

myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : map f xs

powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = powerset xs ++ myMap (x:) (powerset xs)

--Problem 7 

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) = if x <= y
                      then x : merge xs (y:ys)
                      else y : merge (x:xs) ys

mySplitAt :: Int -> [a] -> ([a],[a])
mySplitAt _ [] = ([],[])
mySplitAt _ [x] = ([x],[])
mySplitAt n xs 
    | n <= 0 = (xs,[])
mySplitAt n xs = ((myTake (n) xs),(myDrop (n) xs))

myLength :: [a] -> Int
myLength [] = 0
myLength [x] = 1
myLength (x:xs) = 1 + (myLength xs)

mergesort :: Ord a => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xs = let (as, bs) = mySplitAt ((myLength xs) `quot` 2) xs
               in merge (mergesort as) (mergesort bs)

--Problem 8

countElems :: Eq a => a -> [a] -> Int
countElems _ [] = 0
countElems x [y] 
    | x == y = 1
    | otherwise = 0
countElems x (y:ys)
    | (x == y) = (1 + countElems x ys)
    | otherwise = (countElems x ys)

numberListOfElems :: Eq a => [a] -> [Int]
numberListOfElems [] = [0]
numberListOfElems [x] = [1]
numberListOfElems (x:xs) = countElems x (x:xs) : numberListOfElems xs

mostCommonElement :: Eq a => [a] -> a
mostCommonElement [] = error "empty list"
mostCommonElement [x] = x
mostCommonElement (x:xs) = let n = countElems x (x:xs)
                               m = myMaximum (numberListOfElems (x:xs))
                           in if(n >= m) then x else mostCommonElement xs
