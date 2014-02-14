{-# LANGUAGE MultiWayIf, BangPatterns #-}

module Optimization.SimulatedAnnealing (
    aSimulatedAnnealing
  , MetricSpace (..)
  , SmA (..)
  , config
  , rGen, kmax, emax
  , module Optimization.FrameWork
) where

--
-- Simulated Annealing algorithm as described in 
--   "Numerical Recipes: The Art of Scientific Computing" 
--
-- by William H. Press, Saul A. Teukolsky, William T. Vetterling and Brian P. Flannery
--

import Optimization.Internal
import Optimization.FrameWork
import Prelude hiding ( last, null )
import System.IO.Unsafe ( unsafeInterleaveIO )
import Data.IORef
import System.Random
import Control.Monad
import Control.Applicative ( (<$>) )

class Random a => SmA a where
    pickNearBy :: RandomGen g => g -> a -> (a, g)

instance SmA Double where
    pickNearBy g x = let (x', g') = randomR (-1, 1) g in (x + x', g')

class MetricSpace x where
    dist :: x -> x -> Double

instance MetricSpace a => MetricSpace (SampleAt a b) where
    dist (SampleAt x1 _) (SampleAt x2 _) = dist x1 x2

instance MetricSpace Double where
    dist x1 x2 = abs $ x2 - x1

aSimulatedAnnealing :: (SmA a, MetricSpace r, Ord r) => [a] -> OptimizationStrategyM IO a r
aSimulatedAnnealing = aSimulatedAnnealingWith config

{-# SPECIALIZE aSimulatedAnnealingWith :: (RandomGen g, SmA a) => Config g Double -> [a] -> OptimizationStrategyM IO a Double #-}
aSimulatedAnnealingWith :: (RandomGen g, SmA a, MetricSpace r, Ord r) => Config g r -> [a] -> OptimizationStrategyM IO a r
aSimulatedAnnealingWith conf as = OptimizationStrategyM opt
    where
        opt c = do
          s  <- rGen conf >>= newIORef
          t  <- newIORef return
          t' <- newIORef return

          let particles    = map (particle t') as
              particle t a = do 
                 st <- newIORef (a, a) 
                 return $ do
                    x <- readIORef st
                    m <- readIORef t
                    x' <- m x 
                    writeIORef st x' >> return (return (fst x'))

          ps <- many' particles

          let energyOf   = last
              compute' c = do 
                 readIORef t >>= writeIORef t' 
                 writeIORef t return 
                 let genSamples (p:ps) = p `timesM` genSamples ps
                 c `computeWith` genSamples ps

              setState    = modifyIORef' t (>=> set)
              randomState = modifyIORef' t (>=> permuteWith s)
              prevState   = modifyIORef' t (>=> rollBack)
              nextState c = do 
                 s <- randomState >> compute' c
                 if | null s    -> prevState >> nextState c
                    | otherwise -> return s
            
              run !k !e !m | k > kmax conf                   = return $ Just m
                           | False `maybe` (e >) $ emax conf = return $ Just m
                           | otherwise                       = do
                                g' <- readIORef s
                                let (r, g'') = randomR (0, 1) g'
                                writeIORef s g''

                                s' <- nextState c
                                let e' = energyOf s'
                                    m' = max m e'
                                    t' = temperatureAt k

                                if | acceptanceProb e e' t' > r -> setState  >> run (k + 1) e' m'
                                   | otherwise                  -> prevState >> run (k + 1) e  m'
                            
          s' <- compute' c
          when (null s') $ error "initial state is not in the domain"
          let e' = energyOf s'
          run 0 e' e' 

        set (x, _y)      = return (x, x)
        rollBack (_x, y) = return (y, y)
        permuteWith s (_x, y) = do 
           (y', g') <- flip pickNearBy y <$> readIORef s
           writeIORef s g' >> return (y', y)
            
        -- | Temperature
        temperatureAt x = 1 - x / kmax conf
        -- | Acceptance probability
        acceptanceProb e1 e2 t = exp $ - dist e1 e2 / (boltzmann * t)
        -- | Global params
        boltzmann = 1e7

        many' :: [IO a] -> IO [a]
        many' []     = return []
        many' (a:as) = do
           x  <- a
           xs <- unsafeInterleaveIO $ many' as
           return $ x:xs

data Config g r = C { rGen :: IO g, emax :: Maybe r, kmax :: Double }

-- Default configuration for SimulatedAnnealing
config = C getStdGen Nothing 1000
