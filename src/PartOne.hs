module PartOne (myLast) where

myLast :: [a] -> a
myLast [] = error "no last elem on empty list"
myLast [x] = x
myLast (_ : xs) = myLast xs