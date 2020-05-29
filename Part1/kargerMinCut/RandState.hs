module RandState where

import System.Random

newtype RandState a = RandState {
  runRandState :: StdGen -> (a, StdGen)
}

instance Functor RandState where
  fmap f (RandState r) = RandState $ \s0 -> 
        let (a, s1) = r s0
        in (f a, s1)
      
instance Applicative RandState where
  pure a = RandState $ (\s -> (a,s))
  (<*>) (RandState sa) (RandState sb) =
    RandState (\s0 -> let (fn, s1) = sa s0
                          (a,  s2) = sb s1
                      in (fn a, s2))

instance Monad RandState where
  (>>=) (RandState sa) fn =
   RandState (\s0 -> let (a, s1)  = sa s0
                         RandState sb = fn a
                 in sb s1)

{- primitive manipulation functions -}

get :: RandState StdGen
get = RandState $ \gen -> (gen, gen)

put :: StdGen -> RandState ()
put gen' = RandState $ \gen -> ((), gen')

-- runRandom runs a RandState monad, given an initial random number generator
-- runRandom is equivalent to evalState
runRandom :: RandState a -> StdGen -> a
runRandom (RandState f) s = fst $ f s

-- rand is a helper function that generates a random instance of any
--  type in the Random class, using the RandState monad.
rand :: Random a => RandState a
rand = do
    gen <- get
    let (x, gen') = random gen
    put gen'
    return x
