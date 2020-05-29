

newtype State s a = State { runState :: s -> (a,s) }

{-
instance Functor (State s) where
 fmap f sm = State $ \s -> 
  let (a',s') = runState sm s
  in (f a', s')
-}

instance Functor (State s) where
 fmap f sm = State $ (\(a,s) -> (f a,s)) . runState sm

{-
instance Applicative (State s) where
 pure a = State $ \s -> (a,s)
 fs <*> as = State $ \s ->
  let (f,s')  = runState fs s
      (b,s'') = runState as s'
  in (f b,s'')
-}

instance Applicative (State s) where
 pure a = State $ (,) a
 fs <*> as = State $ (\(f,(b,s'')) -> (f b,s'')) . (\(f,s') -> (f,runState as s')) . runState fs

instance Monad (State s) where
 return = pure
 sa >>= f = State $ \s -> 
  let (a,s') = runState sa s
  in runState (f a) s' 
