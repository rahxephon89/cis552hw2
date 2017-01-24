{-# OPTIONS -fwarn-incomplete-patterns -fwarn-tabs -fno-warn-type-defaults #-}

-- {-# OPTIONS -fdefer-type-errors  #-}

module Main where
import Prelude hiding (takeWhile, all, concat)
import Test.HUnit      -- unit test support

import XMLTypes        -- support file for XML problem (provided)
import Play            -- support file for XML problem (provided)

doTests :: IO ()
doTests = do 
  _ <- runTestTT $ TestList [ testFoldr, testTree, testXML ]
  return ()

main :: IO ()
main = do 
       doTests
       return ()

----------------------------------------------------------------------

testFoldr :: Test
testFoldr = TestList [tintersperse, tinvert, ttakeWhile, tfind, tall, tmap2, tconcat, tmapMaybe]

-- The intersperse function takes an element and a list 
-- and `intersperses' that element between the elements of the list. 
-- For example,
--    intersperse ',' "abcde" == "a,b,c,d,e"

intersperse ::  a -> [a] -> [a]
intersperse a [] = []
intersperse a (x:xs) = foldr (myFunc a) [] (x:xs)

myFunc :: a -> a -> [a] -> [a]
myFunc _ a [] = [a]
myFunc c a (x:xs) = a:c:x:xs





tintersperse :: Test
tintersperse = "intersperse" ~: assertFailure "testcase for intersperse"


-- invert lst returns a list with each pair reversed. 
-- for example:
--   invert [("a",1),("a",2)] returns [(1,"a"),(2,"a")] 

invert :: [(a,b)] -> [(b,a)]
invert = undefined
tinvert :: Test
tinvert = "invert" ~: assertFailure "testcase for invert"
 

-- takeWhile, applied to a predicate p and a list xs, 
-- returns the longest prefix (possibly empty) of xs of elements 
-- that satisfy p:
-- For example, 
--     takeWhile (< 3) [1,2,3,4,1,2,3,4] == [1,2]
--     takeWhile (< 9) [1,2,3] == [1,2,3]
--     takeWhile (< 0) [1,2,3] == []

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile = undefined
ttakeWhile :: Test
ttakeWhile = "takeWhile" ~: assertFailure "testcase for takeWhile"
 

-- find pred lst returns the first element of the list that 
-- satisfies the predicate. Because no element may do so, the 
-- answer is returned in a "Maybe".
-- for example: 
--     find odd [0,2,3,4] returns Just 3

find :: (a -> Bool) -> [a] -> Maybe a
find = undefined
tfind :: Test
tfind = "find" ~: assertFailure "testcase for find"
 

-- all pred lst returns False if any element of lst 
-- fails to satisfy pred and True otherwise.
-- for example:
--    all odd [1,2,3] returns False

all  :: (a -> Bool) -> [a] -> Bool
all = undefined
tall :: Test
tall = "all" ~: assertFailure "testcase for all"
 

-- map2 f xs ys returns the list obtained by applying f to
-- to each pair of corresponding elements of xs and ys. If
-- one list is longer than the other, then the extra elements
-- are ignored.
-- i.e.
--   map2 f [x1, x2, ..., xn] [y1, y2, ..., yn, yn+1]
--        returns [f x1 y1, f x2 y2, ..., f xn yn]
--
-- NOTE: map2 is called zipWith in the Prelude

map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 = undefined

tmap2 :: Test
tmap2 = "map2" ~: assertFailure "testcase for map2"  

-- concat
 
-- The concatenation of all of the elements of a list of lists
-- for example:
--    concat [[1,2,3],[4,5,6],[7,8,9]] returns [1,2,3,4,5,6,7,8,9]
--
-- NOTE: remember you cannot use any functions from the Prelude or Data.List for
-- this problem, even for use as a helper function.
 

concat :: [[a]] -> [a]
concat = undefined
 
tconcat :: Test
tconcat = "concat" ~: assertFailure "testcase for concat"  

-- mapMaybe
 
-- Map a partial function over all the elements of the list
-- for example:
--    mapMaybe root [0.0, -1.0, 4.0] == [0.0,2.0]

root :: Double -> Maybe Double
root d = if d < 0.0 then Nothing else Just $ sqrt d

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe = undefined
 
tmapMaybe :: Test
tmapMaybe = "mapMaybe" ~: assertFailure "testcase for mapMaybe"

----------------------------------------------------------------------

testTree :: Test
testTree = TestList [ tinvertTree, ttakeWhileTree, tallTree, tmap2Tree ]

-- | a basic tree data structure
data Tree a = Leaf | Branch a (Tree a) (Tree a) deriving (Show, Eq)

foldTree :: b -> (a -> b -> b -> b) -> Tree a -> b
foldTree e _ Leaf     = e
foldTree e n (Branch a n1 n2) = n a (foldTree e n n1) (foldTree e n n2)

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f = foldTree Leaf (\x t1 t2 -> Branch (f x) t1 t2) 

-- The appendTree function takes two trees and replaces all of the 'Leaf'
-- constructors in the first with the second tree.  For example: 
--     appendTree (Branch 'a' Leaf Leaf) (Branch 'b' Leaf Leaf) returns
--        Branch 'a' (Branch 'b' Leaf Leaf) (Branch 'b' Leaf Leaf)

appendTree :: Tree a -> Tree a -> Tree a
appendTree = undefined
tappendTree :: Test
tappendTree = "appendTree" ~: assertFailure "testcase for appendTree" 
 

-- The invertTree function takes a tree of pairs and returns a new tree 
-- with each pair reversed.  For example:
--     invertTree (Branch ("a",1) Leaf Leaf) returns Branch (1,"a") Leaf Leaf

invertTree :: Tree (a,b) -> Tree (b,a)
invertTree = undefined
tinvertTree :: Test
tinvertTree = "invertTree" ~: assertFailure "testcase for invertTree"
 

-- takeWhileTree, applied to a predicate p and a tree t, 
-- returns the largest prefix tree of t  (possibly empty) 
-- where all elements satisfy p. 
-- For example, given the following tree

tree1 :: Tree Int
tree1 = Branch 1 (Branch 2 Leaf Leaf) (Branch 3 Leaf Leaf)

--     takeWhileTree (< 3) tree1  returns Branch 1 (Branch 2 Leaf Leaf) Leaf
--     takeWhileTree (< 9) tree1  returns tree1
--     takeWhileTree (< 0) tree1  returns Leaf

takeWhileTree :: (a -> Bool) -> Tree a -> Tree a
takeWhileTree = undefined
ttakeWhileTree :: Test
ttakeWhileTree = "takeWhileTree" ~: assertFailure "testcase for takeWhileTree"
 

-- allTree pred tree returns False if any element of tree 
-- fails to satisfy pred and True otherwise.
-- for example:
--    allTree odd tree1 returns False

allTree :: (a -> Bool) -> Tree a -> Bool
allTree = undefined
tallTree :: Test
tallTree = "allTree" ~: assertFailure "testcase for allTree"
 

-- WARNING: This one is a bit tricky!  (Hint: the value
-- *returned* by foldTree can itself be a function.)

-- map2Tree f xs ys returns the tree obtained by applying f to 
-- to each pair of corresponding elements of xs and ys. If 
-- one branch is longer than the other, then the extra elements 
-- are ignored.
-- for example:
--    map2Tree (+) (Branch 1 Leaf (Branch 2 Leaf Leaf)) (Branch 3 Leaf Leaf)
--        should return (Branch 4 Leaf Leaf)

map2Tree :: (a -> b -> c) -> Tree a -> Tree b -> Tree c
map2Tree = undefined

tmap2Tree :: Test
tmap2Tree = "map2Tree" ~: assertFailure "testcase for map2Tree"

----------------------------------------------------------------------




formatPlay :: SimpleXML -> SimpleXML
formatPlay = error "implement formatPlay"
 




firstDiff :: Eq a => [a] -> [a] -> Maybe ([a],[a])
firstDiff [] [] = Nothing
firstDiff (c:cs) (d:ds) 
    | c==d = firstDiff cs ds 
    | otherwise = Just (c:cs, d:ds)
firstDiff cs ds = Just (cs,ds)

-- | Test the two files character by character, to determine whether
-- they match.
testResults :: String -> String -> IO ()
testResults file1 file2 = do 
  f1 <- readFile file1
  f2 <- readFile file2
  case firstDiff f1 f2 of
    Nothing -> return ()
    Just (cs,ds) -> assertFailure msg where
      msg  = "Results differ: '" ++ take 20 cs ++ 
            "' vs '" ++ take 20 ds

testXML :: Test
testXML = TestCase $ do 
  writeFile "dream.html" (xml2string (formatPlay play))
  testResults "dream.html" "sample.html"

