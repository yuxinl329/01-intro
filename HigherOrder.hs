module HigherOrder where

import Data.Char
import Test.HUnit
import Prelude hiding (filter, foldr, map, pred, product, sum)

plus1 :: Int -> Int
plus1 x = x + 1

h :: (Bool -> a) -> [a]
h x = [x True, x False]

minus1 :: Int -> Int
minus1 x = x - 1

funp :: (Int -> Int, Int -> Int)
funp = (plus1, minus1)

funs :: [Int -> Int]
funs = [plus1, minus1]

doTwice :: (a -> a) -> a -> a
doTwice f x = f (f x)

dtTests :: Test
dtTests =
  TestList
    [ doTwice plus1 4 ~?= 6,
      doTwice minus1 5 ~?= 3
    ]

plusn :: Int -> (Int -> Int)
plusn n = f
  where
    f x = x + n

plus10 :: Int -> Int
plus10 = plusn 10

minus20 :: Int -> Int
minus20 = plusn (-20)

plus :: Int -> Int -> Int
plus m n = m + n

plusfive :: Int -> Int
plusfive = plus 5

pfivetest :: Test
pfivetest = plusfive 1000 ~?= 1005

doTwicePlus20 :: Int -> Int
doTwicePlus20 = doTwice (plus 20)

twoArg :: Int -> String -> Bool
twoArg i s = length (s ++ show i) >= 2

oneStringArg :: String -> Bool
oneStringArg = twoArg 3

oneIntArg :: Int -> Bool
oneIntArg = flip twoArg "a"

anonTests :: Test
anonTests =
  TestList
    [ (\x -> x + 1) 100 ~?= 101,
      doTwice (\x -> x + 1) 100 ~?= 102
    ]

plus1' :: Int -> Int
plus1' = \x -> x + 1

anotherFive :: Int
anotherFive = 2 `plus` 3

anotherFour :: Int
anotherFour = doTwice (+ 2) 0

singleton :: a -> [a]
singleton = undefined

singletonTest :: Test
singletonTest = singleton True ~?= [True]

greaterThan10 :: Int -> Bool
greaterThan10 = (10 <)

ex1 :: (a -> a) -> a -> a
ex1 x y = doTwice doTwice x y

ex1Test :: Test
ex1Test = undefined

len :: [a] -> Int
len [] = 0
len (_ : xs) = 1 + len xs

impossible :: a
impossible = undefined

ok1 :: Int
ok1 = impossible + 1

ok2 :: String
ok2 = "Hello" ++ impossible

ok3 :: String
ok3 = if impossible then "a" else "b"

toUpperString :: String -> String
toUpperString [] = []
toUpperString (x : xs) = toUpper x : toUpperString xs

type XY = (Double, Double)

type Polygon = [XY]

shiftXY :: XY -> XY -> XY
shiftXY (dx, dy) (x, y) = (x + dx, y + dy)

shiftPoly :: XY -> Polygon -> Polygon
shiftPoly _ [] = []
shiftPoly d (xy : xys) = shiftXY d xy : shiftPoly d xys

map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x : xs) = f x : map f xs

toUpperString' :: String -> String
toUpperString' xs = map toUpper xs

shiftPoly' :: XY -> Polygon -> Polygon
shiftPoly' d xs = map (shiftXY d) xs

testMap :: Test
testMap =
  TestList
    [ toUpperString' "abc" ~?= toUpperString "abc",
      shiftPoly' (0.5, 0.5) [(1, 1), (2, 2), (3, 3)]
        ~?= shiftPoly (0.5, 0.5) [(1, 1), (2, 2), (3, 3)]
    ]

listIncr :: [Int] -> [Int]
listIncr [] = []
listIncr (x : xs) = (x + 1) : listIncr xs

listIncr' :: [Int] -> [Int]
listIncr' = undefined

sum :: [Int] -> Int
sum [] = 0
sum (x : xs) = x + sum xs

product :: [Int] -> Int
product [] = 1
product (x : xs) = x * product xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f base [] = base
foldr f base (x : xs) = x `f` foldr f base xs

sum', product' :: [Int] -> Int
sum' = foldr (+) 0
product' = foldr (*) 1

testFoldr :: Test
testFoldr =
  TestList
    [ sum' [1, 2, 3] ~?= sum [1, 2, 3],
      product' [1, 2, 3] ~?= product [1, 2, 3]
    ]

len' :: [a] -> Int
len' = undefined

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n -1)

factorial' :: Int -> Int
factorial' n = undefined

filter :: (a -> Bool) -> [a] -> [a]
testFilter :: Test
testFilter =
  TestList
    [ filter (> 10) [1 .. 20] ~?= [11 .. 20],
      filter (\l -> sum l <= 42) [[10, 20], [50, 50], [1 .. 5]] ~?= [[10, 20], [1 .. 5]]
    ]
filter pred = undefined

runTests :: IO Counts
runTests = runTestTT $ TestList [testMap, testFoldr, testFilter]
