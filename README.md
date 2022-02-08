# alphabeta

An alpha-beta game-solver that _looks like_ a minimax solver.

The very straightforward `solve` function in [MiniMax.hs](src/MiniMax.hs) is reused in the `solve`
implementation of [AlphaBeta.hs](src/AlphaBeta.hs). However, AlphaBeta applies a wrapper to the
game values such that eg.
```
minimum [maximum [4,3], maximum [5,x]]
```
and
```
maximum [minimum [4,5], minimum [2,x]]
```
evaluate to `4` without ever evaluating `x` (in particular, even if `x` is ⊥).

When these wrappers are applied to lazily constructed terms in a thunk representing the output
of a minimax solver, this turns out to be exactly equivalent to an alpha-beta solver.
