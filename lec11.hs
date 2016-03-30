prodList :: Integer a => [a] -> a
prodList [] = 0
prodList [x] = x
prodList xs = foldr (*) 1 xs

myLen :: String -> Integer
myLen "" = 0
myLen x = 1 + myLen (tail x) 

lengthsOfStrings :: [String] -> [Integer]
lengthsOfStrings [] = []
lengthsOfStrings xs = map (myLen) xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse [x] = [x]
myReverse xs = foldl (\acc x -> x:acc) [] xs

sumPosFilFold :: [Integer] -> Integer
sumPosFilFold [] = 0
sumPosFilFold xs = let ys = filter (\x -> x >= 0) xs
                   in foldr (+) 0 ys

sumPosFold :: [Integer] -> Integer
sumPosFold [] = 0
sumPosFold xs = foldr (\x acc -> if (x >= 0) then x + acc else acc) 0 xs

positives :: [Integer] -> [Integer]
positives xs = foldr (\x acc -> if(x >= 0) then x:acc else acc) [] xs

foldyFilter :: (a -> Bool) -> [a] -> [a]
foldyFilter f [] = []
foldyFilter f xs = foldl (\acc x -> if (f x) then (x:acc) else acc) [] xs

paritionBy :: Ord a => a -> [a] -> ([a],[a])
partitionBy _ [] = ([],[])
partitionBy x (y:xs) = foldr (\h (first,second) -> if(h < x) then first:h else second:h) y xs  
