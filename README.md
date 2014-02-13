trading-framework-common
========================

# Optimization Framework.
## Rationale.
TODO.
## Example.
You could use [MonadicComprehensions](https://ghc.haskell.org/trac/ghc/wiki/MonadComprehensions)
or do expression in order to define your computation (may be monadic and partial) you want
to maximize. Like

```haskell
import Optimization.SimulatedAnnealing

objective = [ sin x + y * cos z `at` (x, y) 
            |   x ← newVar, y ← newVar, z ← newVar, x*x + y*y <= 1, x*z >= 0 ]
```

To run the optimization you need to choose a strategy like SimulatedAnnealing or GridSearch. For example the following code will optimize your objective using the SimulatedAnnealing strategy.

```haskell
[...]
g <- getStdGen
objective `optimizedWithM` aSimulatedAnnealing g Nothing 100 (repeat 0.0)
[...]
```
