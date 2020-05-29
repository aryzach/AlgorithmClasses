import System.Environment
import System.Exit
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

main = do 
 ls <- fmap lines (readFile "quickSortList.txt")
 print $ snd $ qs1 ls
 print $ snd $ qs2 ls
 print $ snd $ qs3 ls




qs :: Ord a => ([a] -> a) -> [a] -> ([a],Int)
qs _ [] = ([],0)
qs f xs =
 let pivot  = f xs
     first  = [ x | x <- xs, x < pivot]
     second = [ x | x <- xs, x > pivot]
     (call1,num1) = qs f first
     (call2,num2) = qs f second
 in
  (call1 ++ [pivot] ++ call2, length xs + num1 + num2 - 1) 

qs1,qs2,qs3 :: Ord a => [a] -> ([a],Int)
qs1 = qs head

qs2 = qs last

qs3 = qs threeMedian

threeMedian :: Ord a => [a] -> a
threeMedian (a:[]) = a
threeMedian (a:b:[]) = a
threeMedian xs = 
 let a = head xs
     b = last $ take (div2Up $ length xs) xs 
     c = last xs
 in head $ tail $ fst $ qs1 [a,b,c]


div2Up :: Integral a => a -> a
div2Up a
 | even a = a `div` 2
 | otherwise = a `div` 2 + 1
