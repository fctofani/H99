module Part03 where

-- Q21) insert element in a list at a given location
insertAt :: a -> [a] -> Int -> [a]
insertAt x xs q = insertAt' x xs q []
  where
    insertAt' z [] _ _ = [z]
    insertAt' z ys 1 acc = reverse (z : acc) ++ ys
    insertAt' z (y : ys) n acc = insertAt' z ys (n - 1) (y : acc)