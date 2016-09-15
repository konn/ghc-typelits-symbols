GHC Plugin to do some calculation on GHC's type-level symbols
=============================================================

What's this?
------------
`ghc-typelits-symbols` augments haskell with GHC's type-level symbols with basic operations.
Example:

```haskell
{-# LANGUAGE GADTs, OverloadedLabels, TypeFamilies, TypeOperators #-}
{-# LANGUAGE DataKinds, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Symbols.Solver #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
module Main where
import GHC.TypeLits
import GHC.TypeLits.Symbols
import GHC.OverloadedLabels
import Data.Singletons.TypeLits
import Data.Singletons.Prelude.Maybe
import Data.Singletons.Prelude

type n < m = (n + 1) <= m

data Fin (n :: Nat) where
  Fin :: (KnownNat n, n < m) => SNat n -> Fin m

instance Show (Fin n) where
  showsPrec d (Fin sn) = showsPrec d $ natVal sn

instance (ReadInt (FromJust (StripPrefix "o" lab)) < n, KnownSymbol lab)
      => IsLabel lab (Fin n) where
  fromLabel pxy = reifyNat (read $ tail $ symbolVal' pxy) $ \(Proxy :: Proxy k) ->
    {- Some dirty but safe hacks -}

main :: IO ()
main = do
  print (#o1 :: Fin 4)
  -- print (#o5 :: Fin 2) -- Won't compile!
```

Functions
---------

Basic supported operations:

* `ViewSymbol`{.haskell}: type-level function to deconstruct the given symbol as below:
    1. `ViewSymbol "" ~ 'SymNil`
    2. `ViewSymbol "abc" ~ 'SymCons "a"  "bc"`
* Type-level appending operator `(+++)`: `"123" +++ "456" ~ "123456"`

These operations doesn't reduce by GHCi's `:kind!` command, but
GHC simplifies operations on `Symbol`s defined in terms of `ViewSymbol` and `(+++)` are reduced to simpler form as much as possible in the type-check phase.

This package currently provides the following aux functions defined in terms of aboves:

* `ReadInt`
* `Head`
* `Tail`
