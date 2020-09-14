module Basics where    -- source code comments begin with two dashes

import Test.HUnit      -- library imports must come at the beginning

{-
   You can also create multiline comments if you would like to include
   an extended explanation in your file.
-}

ex :: Integer
ex = 3 * (4 + 5)

pat :: Integer -> Integer -> Integer -> Integer
pat a b c = a * (b + c)

i :: Integer
i = 31 * (42 + 56)        -- arbitrarily large integers

ii :: Int
ii = 31 * (42 + 56)        -- word-sized integers (machine dependent)

d :: Double
d = 3.1 * (42 + 5)         -- double precision floating point

cc :: Char
cc = 'a'                -- characters

ss :: String
ss = "abcd"             -- strings

bb :: Bool
bb = True               -- boolean values

u :: ()
u = ()                  -- 'unit' (both type and constant have the same syntax)

pos :: Int -> Bool
pos x = x > 0

arith :: Int -> Int -> Int -> Int
arith x y z = x * (y + z)

plus :: Int -> Int -> Int
plus = (+)

p0 :: Int
p0 = (+) 2 4

p1 :: Int
p1 = 2 `plus` 2

hw :: IO ()
hw = putStr "Hello World!\n"

many :: IO ()
many = do putStr "Hello"     -- each line in the sequence
          putStr " World!"   -- must be an IO action
          putStr "\n"        -- don't forget the newline

many' :: IO ()
many' = do
  putStr "Hello"
  putStr " World!"
  putStr "\n"

query :: IO ()
query = do putStr "What is your name? "
           n <- getLine
           let y :: String
               y = "Welcome to CIS 552 " ++ n
           putStrLn y

query' :: IO ()
query' = do _m <- putStr "What is your name? "
            n <- getLine
            putStrLn ("Welcome to CIS 552 " ++ n)
            _st <- query2
            return ()

query2 :: IO String   -- compare this type to `query` above.
query2 = do putStr "What is your name? "
            n <- getLine
            return n

query2' :: IO String
query2' = do putStr "What is your name? "
             getLine

t1 :: Test
t1 = (3 :: Int) ~?= 1 + 2
-- check that the expected value `3`
-- matches the result of the computation

numTest :: IO Counts
numTest = runTestTT t1

dotest :: IO ()
dotest = do c <- runTestTT ((3 :: Int) ~?= 3)
            print c

tup1 :: (Char, Int)
tup1 = ('a', 5)                         -- the spacing doesn't matter
tup2 :: (Char, Double, Int)
tup2 = ('a', 5.2, 7)                    -- but it is pretty to line
tup3 :: ((Int, Double), Bool)
tup3 = ((7, 5.2), True)                 -- things up in your code

tpat :: (Int, Int, Int) -> Int
tpat (a, b, c) = a * (b + c)

tup4 :: ((Int,Int),Int)
tup4 = ((1,2),3)            -- a pair of a pair and a number

tup5 :: (Int,(Int,Int))
tup5 = (1,(2,3))            -- a pair of a number and a pair

tup6 :: (Int, Int, Int)
tup6 = (1, 2, 3)            -- a three-tuple

pat2 :: ((Int,Int),Int) -> Int
pat2 ((a, b), c) = a * (b + c)

pat3 :: (Int, (Int, Int)) -> Int
pat3 (a, (b, c)) = a * (b + c)

act2 :: (IO (), IO ())
act2 = (putStr "Hello", putStr "Hello")

runAct2 :: IO ()
runAct2 = do
   let (x, y) = act2     -- pattern match in `do` sequences using `let`
   x                     -- run the first action
   y                     -- then run the second

runAct2' :: IO ()
runAct2' = do
   let (x, y) = act2     -- pattern match
   y                     -- run the second action
   x                     -- then run the first

runAct2'' :: IO ()
runAct2'' = do
   let (x, y) = act2     -- pattern match
   x                     -- run the first action
   x                     -- then run it again!

m1 :: Maybe Int
m1 = Just 2       -- the 'Just' tag tells the compiler that we have a value

m2 :: Maybe Int
m2 = Nothing      -- the 'Nothing' tag means there is no value

pat'' :: Maybe Int -> Int
pat'' (Just x) = x
pat'' Nothing  = 2

jn :: Maybe (Maybe a) -> Maybe a
jn (Just (Just x)) = Just x
jn (Just Nothing) = Nothing
jn Nothing = Nothing

jn' :: Maybe (Maybe a) -> Maybe a
jn' = undefined

location :: String -> Maybe String
location "cis501" = Just "Wu & Chen"
location "cis502" = Just "Heilmeier"
location "cis520" = Just "Wu & Chen"
location "cis552" = Just "online"
location _        = Nothing    -- wildcard pattern, matches anything

l1 :: [Double]
l1 = [1.0,2.0,3.0,4.0]

l2 :: [Int]
l2 = undefined -- make a list of numbers

l3 :: [(Int,Bool)]
l3 = [ (1,True), (2, False) ]

l4 :: [[Int]]
l4 = undefined -- make a list of lists

-- l5 :: [Int]
-- l5 = [ 1 , True ]  -- doesn't type check

l6 :: [a]
l6 = []

l7 :: String
l7 = ['h','e','l','l','o',' ','5','5','2','!']

cons :: a -> [a] -> [a]
cons = (:)

c1 :: [Bool]
c1 = True : [False, False]

c2 :: [Int]
c2 = 1 : []

-- undefined: fill in the type of c3
c3 = [] : []

s1 :: [Char]
s1 = "abc"
 
s2 :: String
s2 = ['a', 'b', 'c']

testClone1, testClone2, testClone3, testClone4 :: Test
testClone1 = clone 'a' 4 ~?= ['a','a','a','a']
testClone2 = clone 'a' 0 ~?= []
testClone3 = clone (1.1 :: Double) 3 ~?= [1.1, 1.1, 1.1]
testClone4 = clone 'a' (-1) ~?= []

clone :: a -> Int -> [a]

clone x n = if n<=0 then [] else x : clone x (n-1)

cl1, cl2, cl3, cl4 :: IO Counts
cl1 = runTestTT testClone1
cl2 = runTestTT testClone2
cl3 = runTestTT testClone3
cl4 = runTestTT testClone4

cls :: IO Counts
cls = runTestTT (TestList [ testClone1, testClone2, testClone3, testClone4 ])

testRange :: Test
testRange = TestList [ range 3  6  ~?= [3,4,5,6],
                       range 42 42 ~?= [42],
                       range 10 5  ~?= [] ]

range :: Int -> Int -> [Int]

range i j = if i>j then [] else i : range (i+1) j

runRTests :: IO Counts
runRTests = runTestTT testRange

isHi :: String -> Bool
isHi ['H','i'] = True
isHi _ = False

isGreeting :: String -> Bool
isGreeting "Hi" = True
isGreeting "Hello" = True
isGreeting "Bonjour" = True
isGreeting "Guten Tag" = True
isGreeting _ = False

isSingleton :: [a] -> Bool
isSingleton [_] = True
isSingleton _   = False

isLong :: [a] -> Bool
isLong [] = False
isLong l = length l >= 2

testIsLong :: Test
testIsLong = TestList [ not (isLong [])   ~? "nil",    -- can convert booleans to tests by naming them via `~?`
                        not (isLong "a")  ~? "one",
                        not (isLong "ab") ~? "two",
                        isLong "abc"      ~? "three" ]

runTestIsLong :: IO Counts
runTestIsLong = runTestTT testIsLong

listAddTests :: Test
listAddTests = TestList [ listAdd [1,2,3] ~?= 6,
                          listAdd [] ~?= 0 ]

listAdd :: [Int] -> Int

listAdd []       = 0
listAdd (x : xs) = x + listAdd xs 

runLATests :: IO Counts
runLATests = runTestTT listAddTests

listIncrTests :: Test
listIncrTests =
 TestList [ listIncr [1,2,3] ~?= [2,3,4],
            listIncr [42]    ~?= [43] ]

listIncr :: [Int] -> [Int]

listIncr [] = []
listIncr (x : xs) = x + 1 : listIncr xs

runLITests :: IO Counts
runLITests = runTestTT listIncrTests

