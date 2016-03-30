aList = [7,3,1,5,9]
bList = [2,4]
cList = append bList aList
append :: [a] -> [a] -> [a]
append [] [] = []
append [] lst = lst
append lst [] = lst
append (x:xs) ys = x:(append xs ys)

fillySunc (x:y:ys) = (x:7:xs)
fillySunc lst = lst

toTree :: Ord a => [a] -> Tree a
toTree [] = Leaf
toTree (x:xs) = insert x (toTree xs) 
