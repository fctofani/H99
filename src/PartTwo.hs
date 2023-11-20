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

decodeModified :: (Eq a) => [EncodeTuple a] -> [a]
decodeModified [] = []
decodeModified ((Single x) : xs) = x : decodeModified xs
decodeModified ((Multiple 1 x) : xs) = x : decodeModified xs
decodeModified ((Multiple n x) : xs) = x : decodeModified (Multiple (n - 1) x : xs)