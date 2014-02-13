{-# LANGUAGE BangPatterns #-}

module Optimization.GridSearch (
   aGridSearch
 , module Optimization.FrameWork
)where 

import Optimization.Internal
import Optimization.FrameWork
import Prelude hiding (maximum)

aGrid :: (Ord a, Fractional a) => a -> a -> Int -> Samples a
aGrid x y n = Samples $ \f a -> go f a y
    where
        d = let n' = fromRational $ toRational n in (y - x) / n'
        go !f !a' !y' | y' < x    = a'
                      | otherwise = go f (f y' a') (y' - d)

aGridSearch :: (Monad m, Ord a, Fractional a, Ord r) => a -> a -> Int -> OptimizationStrategyM m a r
aGridSearch a b n = let grid = aGrid a b n
                        infitelyMany g = g `times` infitelyMany g
                     in OptimizationStrategyM $ \e -> maximum $ e `computeWith` infitelyMany grid
