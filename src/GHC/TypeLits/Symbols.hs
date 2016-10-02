{-# LANGUAGE DataKinds, EmptyDataDecls, ExplicitNamespaces, GADTs #-}
{-# LANGUAGE TemplateHaskell, TypeFamilies, TypeOperators         #-}
{-# LANGUAGE UndecidableInstances                                 #-}
-- | Use this module with the following options:
--
--  @{-\# GHC_OPTIONS -fplugin GHC.TypeLits.Symbols.Solver \#-}@
module GHC.TypeLits.Symbols
       (Symbol, type (+++), Head, Tail, ReadInt, Null,
        ToCharList, StripPrefix,
        -- * Low-level deconstruction
        -- | Data-constructors of @'SymbolView'@ are exposed
        -- in "GHC.TypeLits.Symbols.Internal".
        ViewSymbol,
        SymbolView) where
import GHC.TypeLits.Symbols.Internal

import Data.Promotion.TH
import Data.Singletons.Prelude.List (Foldl1, Map)
import Data.Singletons.Prelude.Num
import Data.Singletons.TypeLits

type family Null (s :: Symbol) :: Bool where
  Null "" = 'True
  Null _  = 'False

-- | @'Head' s@ takes the head character of @s :: 'Symbol'@.
--
-- @
-- 'Head' "123" ~ "1"
-- @
type Head s = Head' (ViewSymbol s)
type family Head' (v :: SymbolView) :: Symbol where
  Head' ('SymCons x y) = x

-- | @'Tail' s@ takes the tail part of @s :: 'Symbol'@.
--
-- @
-- 'Tail' "123" ~ "23"
-- @
type Tail s = Tail' (ViewSymbol s)
type family Tail' (v :: SymbolView) :: Symbol where
  Tail' ('SymCons x y) = y

-- | @'ToCharList' s@ breaks the symbol @s@ into the type-level list
--   consisting of symbols which contains exactly one character.
--
-- @
-- 'ToCharList' "" ~ '[]
-- 'ToCharList' "123" ~ '["1", "2", "3"]
-- @
type ToCharList (s :: Symbol) = ToCharList' (ViewSymbol s)

type family ToCharList' (sv :: SymbolView) :: [Symbol] where
  ToCharList' 'SymNil = '[]
  ToCharList' ('SymCons s ss) = s ': ToCharList' (ViewSymbol ss)

promoteOnly [d|
  shift :: Nat -> Nat -> Nat
  shift a b = a * 10 + b

  readDigit :: Symbol -> Nat
  readDigit "0" = 0
  readDigit "1" = 1
  readDigit "2" = 2
  readDigit "3" = 3
  readDigit "4" = 4
  readDigit "5" = 5
  readDigit "6" = 6
  readDigit "7" = 7
  readDigit "8" = 8
  readDigit "9" = 9
 |]

-- | @'ReadInt' s@ converts decimal @s@ into @'Nat'@.
type ReadInt s = Foldl1 ShiftSym0 (Map ReadDigitSym0 (ToCharList s))

type family Recons v where
  Recons 'SymNil = ""
  Recons ('SymCons s t) = s +++ t

-- | @'StripPrefix' prf str@ tries to strip the prefix @prf@ from the given symbol @str@.
--
-- @
-- 'StripPrefix' "foo" "foobar" ~ ''Just' "bar"
-- 'StripPrefix' "foo" "barbaz" ~ ''Nothing'
-- @
type StripPrefix prf s = StripPrefix' (ViewSymbol prf) (ViewSymbol s)
type family StripPrefix' v u :: Maybe Symbol where
  StripPrefix' 'SymNil s = 'Just (Recons s)
  StripPrefix' ('SymCons p ps) ('SymCons p qs) = StripPrefix' (ViewSymbol ps) (ViewSymbol qs)
  StripPrefix' x y = 'Nothing

