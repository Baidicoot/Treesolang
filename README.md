# Treesolang
A small esolang inspired by the haskell type system and pattern matching.

https://esolangs.org/wiki/Treesolang

Note that this implementation does not have I/O and instead outputs the fully normalised state. Additionally, any patterns of the form `f x` will not parse, because I build type inference wrongly and can't be bothered to fix it.

## Building
Requires GHC; https://www.haskell.org/ghc/

This command *might* work (idk if I got the `LANGUAGE` pragmas right). Execute in the main folder:
```
ghc Main.hs
```
if that fails, run:
```
ghc Main.hs -XGADTs -XFlexibleInstances
```
