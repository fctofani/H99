module PartTwo where

-- Q11) modified encoding with non-dupes being simply added to the list
data EncodeTuple a = Single a | Multiple Integer a deriving (Eq, Show)

encodeModified :: (Eq a) => [a] -> [EncodeTuple a]
encodeModified xs = encodedModified' xs 0 []
  where
    encodedModified' :: (Eq a) => [a] -> Integer -> [EncodeTuple a] -> [EncodeTuple a]
    encodedModified' [] _ acc = acc
    encodedModified' [y] 0 acc = reverse (Single y : acc)
    encodedModified' [y] n acc = reverse (Multiple (n + 1) y : acc)
    encodedModified' (y : ys) n acc
      | y == head ys = encodedModified' ys (n + 1) acc
      | n == 0 = encodedModified' ys 0 (Single y : acc)
      | otherwise = encodedModified' ys 0 (Multiple (n + 1) y : acc)

-- Q12) decode an encoded list
decodeModified :: (Eq a) => [EncodeTuple a] -> [a]
decodeModified [] = []
decodeModified ((Single x) : xs) = x : decodeModified xs
decodeModified ((Multiple 1 x) : xs) = x : decodeModified xs
decodeModified ((Multiple n x) : xs) = x : decodeModified (Multiple (n - 1) x : xs)

-- Q14) duplicate list elements
dupli :: [a] -> [a]
dupli [] = []
dupli (x : xs) = x : x : dupli xs

-- Q15) replicate list elements n times
repli :: [a] -> Int -> [a]
repli [] _ = []
repli _ 0 = []
repli xs q = repli' xs q 0
  where
    repli' :: [a] -> Int -> Int -> [a]
    repli' [] _ _ = []
    repli' (y : ys) n m
      | n > m = y : repli' (y : ys) n (m + 1)
      | otherwise = repli' ys n 0

-- Q16) drop every nth element from a list
dropEvery :: [a] -> Int -> [a]
dropEvery xs q = dropEvery' xs q 1
  where
    dropEvery' :: [a] -> Int -> Int -> [a]
    dropEvery' [] _ _ = []
    dropEvery' (y : ys) n m
      | n > m = y : dropEvery' ys n (m + 1)
      | otherwise = dropEvery' ys n 1

-- Q17) split a list in two parts based on the length of the first part
split :: [a] -> Int -> ([a], [a])
split xs q = split' xs q []
  where
    split' :: [a] -> Int -> [a] -> ([a], [a])
    split' [] _ acc = (reverse acc, [])
    split' ys 0 acc = (reverse acc, ys)
    split' (y : ys) n acc = split' ys (n - 1) (y : acc)

-- Q18) extract a slice from a list
slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice _ _ 0 = []
slice (x : xs) 1 n = x : slice xs 1 (n - 1)
slice (_ : xs) m n = slice xs (m - 1) (n - 1)

-- Q19) rotate a list
rotate :: [a] -> Int -> [a]
rotate xs q = rotate' xs q []
  where
    rotate' :: [a] -> Int -> [a] -> [a]
    rotate' [] _ acc = acc
    rotate' ys 0 acc = acc ++ ys
    rotate' (y : ys) 1 acc = ys ++ reverse (y : acc)
    rotate' (y : ys) n acc
      | n > 1 = rotate' ys (n - 1) (y : acc)
      | otherwise = rotate' (init (y : ys)) (n + 1) (last (y : ys) : acc)