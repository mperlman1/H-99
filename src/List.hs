module List where

--Problem 1
myLast :: [a] -> a
myLast [] = error "No last element in an empty list."
myLast [x] = x
myLast (_:xs) = myLast xs

--Problem 2
myButLast :: [a] -> a
myButLast [] = error "No penultimate element in an empty list."
myButLast [x] = error "No penultimate element in a single element list."
myButLast [x,_] = x
myButLast (_:xs) = myButLast xs

--Problem 3
elementAt :: [a] -> Int -> a
elementAt [] _ = error "Cannot take kth element of an empty list."
elementAt (x:_) 1 = x
elementAt (_:xs) k = elementAt xs (k - 1)

--Problem 4
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = myLength xs + 1

--Problem 5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

--Problam 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == myReverse xs

-- Problem 7
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (List []) = []
flatten (Elem x) = [x]
flatten (List (x:xs)) = (flatten x) ++ (flatten $ List xs)


--Problem 8
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x,y]
  | x == y = [x]
  | otherwise = [x,y]
compress (x:y:zs)
  | x == y = compress (x : zs)
  | otherwise = x : compress (y : zs)

--Problem 9
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack xs = foldl pack' [[]] xs
  where pack' [[]] x = [[x]]
        pack' a x = if myLast (myLast a) == x
                    then init a ++ [(x : myLast a)]
                    else a ++ [[x]]

--Problem 10
encode :: (Eq a) => [a] -> [(Int, a)]
encode [] = []
encode xs = map encode' (pack xs)
  where encode' x = (myLength x, myLast x)

--Problem 11
data RLE a = Multiple Int a | Single a
  deriving Show

encodeModified :: (Eq a) => [a] -> [RLE a]
encodeModified [] = []
encodeModified xs = map encodeModified' (pack xs)
  where encodeModified' x
          | myLength x == 1 = Single (myLast x)
          | otherwise = Multiple (myLength x) (myLast x)

--Problem 12
decodeModified :: [RLE a] -> [a]
decodeModified (Single x:xs) = x : decodeModified xs
decodeModified (Multiple n x:xs) = (take n $ repeat x) ++ decodeModified xs
decodeModified [] = []

--Problem 13
encodeDirect :: (Eq a) => [a] -> [RLE a]
encodeDirect [] = []
encodeDirect xs = foldl encodeDirect' [] xs
  where encodeDirect' [] x = [Single x]
        encodeDirect' a x = undefined

--Problem 14
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = [x,x] ++ dupli xs

--Problem 15
repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) n = (take n $ repeat x) ++ repli xs n

--Problem 16
dropEvery:: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery xs n = foldl dropEvery' [] xs
  where dropEvery' a x = undefined

--Problem 17
split :: [a] -> Int -> [[a]]
split xs n = [(take n xs), (drop n xs)]

--Problem 18
slice :: [a] -> Int -> Int -> [a]
slice xs start end = take (end - start + 1) $ drop (start - 1) xs

--Problem 19
rotate :: [a] -> Int -> [a]
rotate xs n
  | n > 0 = (drop n xs) ++ (take n xs)
  | n < 0 = (drop (length xs + n) xs) ++ (take (length xs + n) xs)
  | n == 0 = xs

--Problem 20
removeAt :: Int -> [a] -> (a,[a])
removeAt k xs = (xs !! (k - 1), (take (k - 1) xs) ++ (drop k xs))

--Problem 21
insertAt :: a -> [a] -> Int -> [a]
insertAt x xs k = (take (k - 1) xs) ++ [x] ++ (drop (k - 1) xs)

--Problem 22
range :: Int -> Int -> [Int]
range start end
  | start == end = [end]
  | otherwise = start : range (start + 1) end
