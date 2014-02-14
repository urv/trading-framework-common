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

You can attach some additional parameters that you are interested in to the
output by using the `at` combinator. In this case you get back the value and
parameters - otherwise just the maximum value will be returned.  To run the
optimization you need to choose a strategy like SimulatedAnnealing or
GridSearch. For example the following code will optimize your objective using
the SimulatedAnnealing strategy.

```haskell
[...]
Just o <- objective `optimizedWithM` aSimulatedAnnealing (repeat 0.0)
print $ value o
print $ parameters o
[...]
```
