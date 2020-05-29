import RandState
import System.Environment
import System.IO
import System.Random

-- Data types to represent playing cards
data CardValue
    = King
    | Queen
    | Jack
    | NumberCard Int  -- From 1 to 10
    deriving (Show, Eq)

data CardSuit
    = Hearts
    | Diamonds
    | Spades
    | Clubs
    deriving (Show, Eq)

data PlayingCard =
    PlayingCard CardValue CardSuit
    deriving (Eq)

type Deck = [PlayingCard]


instance Show PlayingCard where
    show (PlayingCard value suit) =
        valueStr value ++ suitStr suit
        where
            suitStr Hearts   = "\x1B[31m♥\x1B[0m" -- red in terminal
            suitStr Diamonds = "\x1B[31m♦\x1B[0m" -- red in terminal
            suitStr Spades   = "♠"
            suitStr Clubs    = "♣"
            -- suitStr Hearts   = "H"  -- uncomment if you don't have Unicode
            -- suitStr Diamonds = "D"
            -- suitStr Spades   = "S"
            -- suitStr Clubs    = "C"
            valueStr King           = "K"
            valueStr Queen          = "Q"
            valueStr Jack           = "J"
            valueStr (NumberCard n) = show n


-- fullCardDeck is a deck of cards, 52 in total, with a King, a Queen,
-- a Jack and NumberCards from 1 to 10 for each suit.
fullCardDeck :: Deck
fullCardDeck =
    [ PlayingCard v s | v <- allVals, s <- allSuits ]
    where
        allVals  = King : Queen : Jack : [ NumberCard i | i <- [1..10] ]
        allSuits = [Hearts, Diamonds, Spades, Clubs]

-- basic random functions

randR :: Random a => (a, a) -> RandState a
randR t = do
    gen <- get
    let (x, gen') = randomR t gen
    put gen'
    return x


randR' :: Random a => (a, a) -> RandState a
randR' t = get >>= \g -> let (a,s) = (randomR t g) in put s >> return a 


rollTwoDice :: RandState Int
rollTwoDice = do
 x <- randR (1,6)
 y <- randR (1,6)
 return $ x+y 

rollTwoDice' :: RandState Int
rollTwoDice' = 
 randR (1,6) >>= \x -> randR (1,6) >>= \y -> return (x+y)

removeCard :: Deck -> RandState (PlayingCard,Deck)
removeCard [] = error "no cards"
removeCard d = do
 num <- (randR (0,length d - 1))
 let card = d !! num    
     restDeck = filter (\x -> x /= card) d
 return (card,restDeck)

{-
removeCard' :: Deck -> RandState (PlayingCard,Deck)
removeCard' [] = error "no cards"
removeCard' d = 
 (randR (0,length d - 1)) >>= (!!) d >>= \card -> filter (\x -> x /= card) d >>= \rd -> return (card,rd)
-}

shuffleDeck :: Deck -> RandState Deck
shuffleDeck [] = return []
shuffleDeck d = do
 (c,d') <- removeCard d
 d'' <- shuffleDeck d'
 return (c : d'')

shuffleADeck :: RandState Deck
shuffleADeck = shuffleDeck fullCardDeck

-- -- UNCOMMENT THE FOLLOWING BEFORE SUBMISSION --
--
--

shuffleNTimes :: Int -> StdGen -> IO ()
shuffleNTimes      0 gen = return ()
shuffleNTimes nTimes gen = do
 let (shuffled,gen') = runRandState shuffleADeck gen 
 print shuffled 
 shuffleNTimes (nTimes - 1) gen'

--
rollTwoDiceNTimes :: Int -> StdGen -> IO ()
rollTwoDiceNTimes      0   _ = return () 
rollTwoDiceNTimes nTimes gen = do
 let (rolled,gen') = runRandState rollTwoDice gen
 print rolled
 rollTwoDiceNTimes (nTimes - 1) gen'
--
--
-- -- BESIDES UNCOMMENTING, DO NOT MODIFY BELOW THIS LINE --
--
usage :: String
usage =
 "Lab 5: Randomizer\n" ++
 "\n" ++
 "$ ./Lab5 shuffle 600      # 600 times: output a full deck shuffle\n" ++
 "$ ./Lab5 rollTwoDice 800  # 800 times: output the sum of rolling two dice\n" ++
 "\n"
--
main :: IO ()
main = do
 gen  <- newStdGen
 args <- getArgs
 case args of
  ["shuffle",     nTimes] -> shuffleNTimes     (read nTimes) gen
  ["rollTwoDice", nTimes] -> rollTwoDiceNTimes (read nTimes) gen
  _                       -> putStrLn usage
