import Text.Read
import System.Random
{-
getRand :: IO Int
getRand = randomIO

thing :: IO Int -> Int
thing x = do
 y <- x
 y
-}

convert :: [[Int]] -> [(Int,[Int])]
convert = map (\x -> (head x,tail x)) 

main :: IO ()
main = do 
 ls <- fmap (\l -> map words l) $ fmap lines (readFile "kargerMinCut.txt")
 let graph = convert $ map (map (read :: String -> Int)) ls 
 print $ graph 
-- g <- getStdGen
-- let final = runAll graph g
{-
runAll :: [[Int]] -> StdGen -> Int
runAll g r = runTimes g r ((length g)^2) []

runTimes :: [[Int]] -> StdGen -> Int -> [Int] -> Int
runTimes _ _ 0 mins = minimum mins 
runTimes g r n mins = runTimes g (snd r) (n-1) (runOnce g (fst r) : mins)


-- int is in vertices
removeOne :: [[Int]] -> Int -> [[Int]]
removeOne xs i = helper xs i []
 where 
  helper [] _ _ = error "list not in list"
  helper xs 0 r = 
-}  

