--
-- Chapter 2, 2.1 Lists
--

module Stack where

import Prelude as P

-- According to the book, methods names in Stack type class use list
-- nomenclature (cons, head, tail) rather then stack ones (push, top, pop),
-- because it regards stacks as an instance of general class of sequences.
class Stack t where
  empty :: t a
  isEmpty :: t a -> Bool

  cons :: a -> t a -> t a

  -- suffix with 'S' (for Stack) to not clash with Haskell Prelude
  headS :: t a -> a
  tailS :: t a -> t a

  -- default implementation of concatenation using functions above
  (+++) :: t a -> t a -> t a
  xs +++ ys = if isEmpty xs then ys
                            else cons (headS xs) (tailS xs +++ ys)

-- Data type using built-in Haskell list type.
data List a = List [a] deriving Show

-- and it implementation of Stack
instance Stack List where
  empty = List []
  isEmpty (List xs) = null xs
  cons x (List xs) = List (x:xs)
  headS (List xs) = P.head xs
  tailS (List xs) = List (P.tail xs)

-- More "raw" data type using tuples.
data CustomList a = Nil | Cons (a, CustomList a) deriving Show

-- and it implementation of Stack
instance Stack CustomList where
  empty = Nil

  isEmpty Nil = True
  isEmpty _ = False

  cons x xs = Cons (x, xs)

  headS Nil = undefined
  headS (Cons (x, _)) = x

  tailS Nil = undefined
  tailS (Cons (_, xs)) = xs

-- Testing methods
main :: IO ()
main = do
  mainList
  mainCustomList

-- Generic tester for Stack type class
mainStack :: (Stack s, Show (s a), Show a) => s a -> s a -> s a -> IO ()
mainStack emptyL xs ys = do
  putStrLn $ "isEmpty empty: " ++ show (isEmpty emptyL)
  putStrLn $ "isEmpty xs: " ++ show (isEmpty xs)
  putStrLn $ "headS xs: " ++ show (headS xs)
  putStrLn $ "tailS xs: " ++ show (tailS xs)
  putStrLn $ "empty +++ xs: " ++ show (emptyL +++ xs)
  putStrLn $ "xs +++ ys   : " ++ show (xs +++ ys)

-- Tester for List data type
mainList :: IO ()
mainList =
  let emptyL = List []
      xs = List [1..6]
      ys = List [7..12]
  in mainStack emptyL xs ys

-- Tester for CustomList data type
mainCustomList :: IO ()
mainCustomList =
  let emptyL = Nil
      xs = Cons (1, Cons (2, Cons (3, Cons (4, Cons (5, Cons (6, Nil))))))
      ys = Cons (7, Cons (8, Cons (9, Cons (10, Cons (11, Cons (12, Nil))))))
  in mainStack emptyL xs ys

