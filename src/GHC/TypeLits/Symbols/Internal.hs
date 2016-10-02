{-# LANGUAGE ExplicitNamespaces #-}
module GHC.TypeLits.Symbols.Internal
       (type (+++) , SymbolView(..), ViewSymbol) where
import GHC.TypeLits (Symbol)

type family (s :: Symbol) +++ (t :: Symbol) :: Symbol where
  "" +++ s  = s
  s  +++ "" = s

data SymbolView = SymNil
                | SymCons Symbol Symbol

-- | @'ViewSymbol' s@ provides per-character view for the symbol @s@.
--
-- @
-- 'ViewSymbol' ""    ~ ''SymNil'
-- 'ViewSymbol' "123" ~ ''SymCons' "1" "23"
-- @
type family ViewSymbol (s :: Symbol) :: SymbolView where
  ViewSymbol "" = 'SymNil

