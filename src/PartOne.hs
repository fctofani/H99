module PartOne (myLast, mySecLast, findElem, myLen) where

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
myLen' :: [a] -> Integer -> Integer
myLen' [] n = n
myLen' (_ : xs) n = myLen' xs (n + 1)

myLen :: [a] -> Integer
myLen xs = myLen' xs 0
