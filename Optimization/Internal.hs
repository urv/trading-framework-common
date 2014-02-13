{-# LANGUAGE FlexibleInstances, MultiWayIf, Rank2Types, TupleSections, BangPatterns #-}

module Optimization.Internal (
    newVar
    , Samples(..)
    , null
    , last
    , SampleAt(..)
    , at
    , OptimizationStrategyM(..)
    , optimizedWith
    , optimizedWithM
    , OptimizationStrategy(..)
    , computeWith
    , timesM
    , times
    , maximum
    ) where

import Prelude hiding ( concat, foldr, last, null, maximum )
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Arrow ( first )
import Data.Functor.Identity
import Data.Foldable ( Foldable(..) )

newtype Samples a = Samples { gen :: forall  b. (a -> b -> b) -> b -> b }

fromList :: [a] -> Samples a
fromList xs = Samples $ \f a -> foldr f a xs

instance Foldable Samples where
    foldr f a s = gen s f a

instance Functor Samples where
    fmap f m = Samples $ \h -> gen m (h . f)

instance Monad Samples where
    n >>= m = Samples $ \f x -> let f' a = gen (m a) f in foldr f' x n
    return a = Samples $ \f x -> f a x

instance Applicative Samples where
    pure = return
    m <*> n = m >>= flip fmap n

instance MonadPlus Samples where
    mzero = nil
    mplus = concat

instance Alternative Samples where
    empty = mzero
    (<|>) = mplus

concat :: Samples a -> Samples a -> Samples a
m `concat` n = Samples $ \f x -> let l = foldr f x n in foldr f l m

nil :: Samples a
nil = Samples $ \f x -> x

last :: Samples a -> a
last = foldr const undefined

null :: Samples a -> Bool
null = foldr (\_ _ -> False) True

maximum :: (Monad m, Ord r) => m (Samples r) -> m (Maybe r)
maximum = liftM (foldr max' Nothing) 
    where
       max' a b = let m = a `seq` b `seq` a `maybe` max a $ b 
                   in m `seq` Just m

newtype SampleGenM m a = SampleGenM (m (Samples a), a -> SampleGenM m a)
type SampleGen = SampleGenM Identity

newtype SampleComputationM m a r = SampleComputationM { compute :: SampleGenM m a -> m (Samples (r, SampleGenM m a)) }
type SampleComputation = SampleComputationM Identity

computeWith :: Monad m => SampleComputationM m a r -> SampleGenM m a -> m (Samples r)
computeWith e g = liftM (fmap fst) $ compute e g

instance Monad m => Functor (SampleComputationM m a) where
    fmap f e = SampleComputationM $ \g -> liftM (fmap (first f)) $ compute e g

instance Monad m => Monad (SampleComputationM m a) where
    n >>= m = SampleComputationM $ compute n >=> (mjoin . fmap (uncurry $ compute . m)) 
    return a = SampleComputationM $ \s -> return $ return (a, s)

instance (Monad m) => Applicative (SampleComputationM m a) where
    pure = return
    f <*> m = f >>= flip fmap m

instance Monad m => MonadPlus (SampleComputationM m a) where
    mzero = SampleComputationM $ const $ return nil
    mplus m n = SampleComputationM $ \g -> liftM2 concat (compute m g) (compute n g)

instance Monad m => Alternative (SampleComputationM m a) where
    empty = mzero
    (<|>) = mplus

instance MonadIO (SampleComputationM IO a) where
    liftIO i = SampleComputationM compute
        where
            compute g = do
                y <- i
                return $ return (y, g)

mjoin :: Monad m => Samples (m (Samples a)) -> m (Samples a)
mjoin = foldr (liftM2 concat) $ return nil
{-# INLINE[1] mjoin #-}
{-# RULES "SpezializeMJoin" forall (s :: Samples (Identity (Samples a))) .  mjoin s = Identity (s >>= runIdentity) 
  #-}

newVar :: Monad m => SampleComputationM m a a
newVar = SampleComputationM $ \s -> let SampleGenM (current, next) = s 
                                     in liftM ((\t -> return (t, next t)) =<<) current

apply :: Monad m => SampleComputationM m a r -> Samples a -> SampleComputationM m a r
apply e s = SampleComputationM $ \g -> compute e $ SampleGenM (return s, const g)

bottom :: Monad m => SampleGenM m a
bottom = SampleGenM (return nil, const bottom)

section :: Monad m => [a] -> SampleGenM m a
section []     = bottom
section (a:as) = SampleGenM (return $ return a, const $ section as)

timesM :: Monad m => m (Samples a) -> SampleGenM m a -> SampleGenM m a
timesM s g = SampleGenM (s, const g)

times :: Monad m => Samples a -> SampleGenM m a -> SampleGenM m a
times = timesM . return

newtype OptimizationStrategyM m a r = OptimizationStrategyM { optimize :: SampleComputationM m a r -> m (Maybe r) }
type OptimizationStrategy = OptimizationStrategyM Identity

optimizedWithM :: (Monad m) => SampleComputationM m a r -> OptimizationStrategyM m a r -> m (Maybe r)
optimizedWithM = flip optimize

optimizedWith :: SampleComputation a r -> OptimizationStrategy a r -> Maybe r
optimizedWith c e = runIdentity $ optimizedWithM c e

just :: (Monad m, Ord r) => Samples a -> OptimizationStrategyM m a r
just s = OptimizationStrategyM $ \e -> maximum $ e `computeWith` (s `times` bottom)

data SampleAt a b = SampleAt { value :: !a, parameters :: !b }

at :: a -> b -> SampleAt a b
at = SampleAt
infix 0 `at`

instance (Show a, Show b) => Show (SampleAt a b) where
    show (SampleAt a b) = show a ++ " at " ++ show b
    
instance Eq a => Eq (SampleAt a b) where
    SampleAt a _ ==  SampleAt a' _ = a == a'

instance Ord a => Ord (SampleAt a b) where
    SampleAt a _ <=  SampleAt a' _ = a <= a'
