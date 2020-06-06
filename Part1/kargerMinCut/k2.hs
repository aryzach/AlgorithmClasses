import Text.Read hiding (get)
import System.Random
import RandState
import System.Environment
import System.IO

type Graph = [(Int,[Int])]

convert :: [[Int]] -> [(Int,[Int])]
convert = map (\x -> (head x,tail x)) 

main :: IO ()
main = do 
 ls <- fmap (\l -> map words l) $ fmap lines (readFile "kargerMinCut.txt")
 let graph = convert $ map (map (read :: String -> Int)) ls 
 --print $ graph 
 stgen1 <- getStdGen
 stgen2 <- getStdGen
 args  <- getArgs 
 let num = 200 
-- runOnce graph num stgen
 runTest graph stgen1 stgen2 num 

{-
shuffleNTimes :: Int -> StdGen -> IO ()
shuffleNTimes      0 gen = return ()
shuffleNTimes nTimes gen = do
 let (shuffled,gen') = runRandState shuffleADeck gen 
 print shuffled 
 shuffleNTimes (nTimes - 1) gen'
-}

runTest :: Graph -> StdGen -> StdGen -> Int -> IO ()
runTest g gen1 gen2 i = helper g gen1 gen2 i [] 
 where helper g _ _ 0 ls = print $ minimum ls
       helper g gen1 gen2 i ls = do
 --this looks really hacky (using gen twice)
         let ((g',gen''),gen') = runRandState (runOnce g gen1) gen2
         helper g gen' gen'' (i-1) (g':ls) 

--runOnce :: Graph -> Int -> StdGen -> Int
--should it always be (n-1) or is sometimes it more? Relates to removeEdge issue
runOnce :: Graph -> StdGen -> RandState (Int,StdGen) 
runOnce g gen = helper g gen (length g)
 where 
  helper g gen 2 = return (getMin g,gen)
  helper g gen n = do
   let (g',gen') = runRandState (removeEdge g) gen
   helper g' gen' (n-1)

getMin :: Graph -> Int
getMin g = 
 let (_,a) = head g
     (_,b) = head $ tail g
 in if length a == length b then length b
    else error "not same"


--not sure if this removes edge correctly, should more edges be deleted when combining?
removeEdge :: Graph -> RandState Graph
removeEdge g = do
 ind1 <- randR (0,length g - 1)
 let (n1,ns1) = g !! ind1 
 ind2 <- randR (0,length ns1 - 1)
 let n2 = ns1 !! ind2
     ns2 = getEdges n2 g
     nsNew = filter (\x -> x /= n1 && x /= n2) $ ns1 ++ ns2
     fullNew = (n2,nsNew)
     noOld = replaceN1reference n1 n2 $ filter (\(x,_) -> x /= n1 && x /=n2) g
 return $ fullNew : noOld

replaceN1reference :: Int -> Int -> Graph -> Graph
replaceN1reference i j = map (\(a,edges) -> (a,map (\z -> if z == i then j else z) edges)) 

combineEdges :: [Int] -> [Int] -> [Int]
combineEdges [] b = b
combineEdges (a:as) b = 
 if a `elem` b then combineEdges as b 
 else combineEdges as (a:b)

--this function isn't right. Dont use head, but also probably wrong beyond that, or just remove/combineEdges is wrong
getEdges :: Int -> Graph -> [Int]
getEdges i g = 
 let list = filter (\(a,edges) -> i == a) g
 in if length list == 0 then error $ (show i) ++ (show g) 
    else snd $ head list

randR :: Random a => (a, a) -> RandState a
randR t = get >>= \g -> let (a,s) = (randomR t g) in put s >> return a 

rollTwoDice :: RandState Int
rollTwoDice = do
 x <- randR (1,6)
 y <- randR (1,6)
 return $ x+y 


{-
runAll :: [(Int,[Int])] -> Int -> StdGen -> IO ()
runAll graph i stgen = helper graph i stgen lowest
 where helper _ 0 _ lowest = print lowest >> return ()
       helper graph n stgen lowest = 
        let runOnce graph st
shuffleNTimes      0 gen = return ()
shuffleNTimes nTimes gen = do
 let (shuffled,gen') = runRandState shuffleADeck gen 
 print shuffled 
 shuffleNTimes (nTimes - 1) gen'
-}

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

