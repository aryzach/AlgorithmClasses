import System.Environment
import System.Exit
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

main = do 
 ls <- fmap lines (readFile "IntegerArray.txt")
 printInv ls

printInv :: [String] -> IO ()
printInv xs = print $ numOfInversions $ map (read::String->Int) xs


numOfInversions :: Ord a => [a] -> Int
numOfInversions as = fst $ helper as
 where helper :: Ord a => [a] -> (Int,[a])
       helper xs = 
        let l = length xs
            h = l `div` 2
        in
         if (l == 1) || (l == 0) then (0,xs)
         else
          let (fn,fl) = helper (take h xs)
              (sn,sl) = helper (drop h xs)
              (pn,ss) = splitInv fl sl 0 [] 
          in ((fn + sn + pn),ss)
                --assumes lists are sorted
          where splitInv :: Ord a => [a] -> [a] -> Int -> [a] -> (Int,[a]) 
                splitInv a [] n l = (n,l ++ a)
                splitInv [] b n l = (n,l ++ b)
                splitInv (a:as) (b:bs) n l = 
                 if (a <= b) then splitInv as (b:bs) n (l ++ [a])
                 else splitInv (a:as) bs (n + (length (a:as))) (l ++ [b])
           

