{-# LANGUAGE ExplicitNamespaces #-}
module GHC.TypeLits.Symbols.Internal
       (type (+++) , SymbolView(..), ViewSymbol) where
import GHC.TypeLits (Symbol)

type family (s :: Symbol) +++ (t :: Symbol) :: Symbol where
  "" +++ s  = s
  s  +++ "" = s

data SymbolView = SymNil
                | SymCons Symbol Symbol

type family ViewSymbol (s :: Symbol) :: SymbolView where
  ViewSymbol "" = 'SymNil

