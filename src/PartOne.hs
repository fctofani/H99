module PartOne where

-- Q1) Get the last element of a list
myLast :: [a] -> a
myLast [] = error "no last elem on empty list"
myLast [x] = x
myLast (_ : xs) = myLast xs

-- Q2) Find second last element of a list
mySecLast :: [a] -> a
mySecLast [] = error "no second last elem on empty list"
mySecLast [_] = error "no second last on one-elem list"
mySecLast [x, _] = x
mySecLast (_ : xs) = mySecLast xs

-- Q3) find the k element on a list (starting at index 1)
findElem :: [a] -> Integer -> a
findElem [] _ = error "not found"
findElem (x : _) 1 = x
findElem (_ : xs) n = findElem xs (n - 1)

-- Q4) find the length of a list
myLen :: [a] -> Integer
myLen xs = myLen' xs 0
  where
    myLen' :: [a] -> Integer -> Integer
    myLen' [] n = n
    myLen' (_ : ys) n = myLen' ys (n + 1)

-- Q5) reverse a list
myReverse :: [a] -> [a]
myReverse xs = myReverse' xs []
  where
    myReverse' :: [a] -> [a] -> [a]
    myReverse' [] acc = acc
    myReverse' (y : ys) acc = myReverse' ys (y : acc)

-- Q6) find if list is a palindrome
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome xs
  | head xs == last xs = isPalindrome $ init $ drop 1 xs
  | otherwise = False

-- Q7) flatten a list
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (List []) = []
flatten (Elem x) = [x]
flatten (List (Elem x : xs)) = x : flatten (List xs)
flatten (List ((List xs) : ys)) = flatten (List xs) ++ flatten (List ys)

-- Q8) compress a list removing repeated elements
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x : xs)
  | x == head xs = compress xs
  | otherwise = x : compress xs

-- Q9) pack consecutive equal elements in sublists
pack :: (Eq a) => [a] -> [[a]]
pack xs = pack' xs [] []
  where
    pack' :: (Eq a) => [a] -> [a] -> [[a]] -> [[a]]
    pack' [] _ acc' = acc'
    pack' [y] [] acc' = reverse $ [y] : acc'
    pack' [y] acc acc' = reverse $ (y : acc) : acc'
    pack' (y : ys) acc acc'
      | y == head ys = pack' ys (y : acc) acc'
      | otherwise = pack' ys [] ((y : acc) : acc')

-- Q10) lenght encoding of a list
encode :: (Eq a) => [a] -> [(Integer, a)]
encode xs = encode' xs 0 []
  where
    encode' :: (Eq a) => [a] -> Integer -> [(Integer, a)] -> [(Integer, a)]
    encode' [] _ acc = acc
    encode' [y] n acc = reverse ((n + 1, y) : acc)
    encode' (y : ys) n acc
      | y == head ys = encode' ys (n + 1) acc
      | otherwise = encode' ys 0 ((n + 1, y) : acc)
