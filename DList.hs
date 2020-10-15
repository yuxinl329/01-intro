{-# LANGUAGE BlockArguments #-}

module DList where

import Test.HUnit

type DList a = [a] -> [a]

list :: [Int]
list = 1 : 2 : 3 : [] -- end is nil

dlist :: DList Int
dlist = \x -> 1 : 2 : 3 : x -- end is "x"

empty :: DList a
empty = id

testEmpty :: Test
testEmpty =
  "empty"
    ~: TestList
      [ empty "a" ~?= "a",
        empty "" ~?= "",
        empty "abc" ~?= ['a', 'b', 'c']
      ]

runTestEmpty :: IO Counts
runTestEmpty = runTestTT testEmpty

singleton :: a -> DList a
singleton x = (x :)

-- singleton (:)

testSingleton :: Test
testSingleton =
  "singleton"
    ~: TestList
      [ singleton 'a' [] ~?= ['a'],
        singleton 'a' ['b', 'c'] ~?= ['a', 'b', 'c']
      ]

runTestSingleton :: IO Counts
runTestSingleton = runTestTT testSingleton

append :: DList a -> DList a -> DList a
append = (.)

-- append x y = \z -> x (y z)

testAppend :: Test
testAppend =
  "append"
    ~: [toList (append (singleton 1) (singleton 2)) ~?= [1, 2]]

runTestAppend :: IO Counts
runTestAppend = runTestTT testAppend

cons :: a -> DList a -> DList a
cons = append . singleton

fromList :: [a] -> DList a
fromList xs = (xs ++)

toList :: DList a -> [a]
toList x = x []

testDList :: IO ()
testDList = do
  _ <-
    runTestTT $
      TestList
        [ toList empty ~?= ([] :: [Char])
        ]
  return ()

-- create list of 10000 characters and take last element; O(n^2)
micro1 :: Char
micro1 = last (t 10000 "")
  where
    t 0 l = l
    t n l = t (n -1) (l ++ "s")

-- O(n)
micro2 :: Char
micro2 = last (toList (t 10000 empty))
  where
    t 0 l = l
    t n l = t (n -1) (l `append` singleton 's')

naiveReverse :: [a] -> [a]
naiveReverse = rev
  where
    rev [] = []
    rev (x : xs) = rev xs ++ [x]

bigList :: [Int]
bigList = [0 .. 10000]

micro3 :: Int
micro3 = last (naiveReverse bigList)

ivoryTowerReverse :: [a] -> [a]
ivoryTowerReverse = foldr (flip (++) . (: [])) []

micro4 :: Int
micro4 = last (ivoryTowerReverse bigList)

dlistReverse :: [a] -> [a]
dlistReverse = toList . rev
  where
    rev [] = empty
    rev (x : xs) = rev xs `append` singleton x

micro5 :: Int
micro5 = last (dlistReverse bigList)

dlistIvoryTowerReverse :: [a] -> [a]
dlistIvoryTowerReverse = toList . foldr (flip append . singleton) empty

micro6 :: Int
micro6 = last (dlistIvoryTowerReverse bigList)

micro7 :: Int
micro7 = last (reverse bigList)
