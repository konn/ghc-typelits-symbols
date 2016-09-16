{-# LANGUAGE DataKinds, EmptyDataDecls, ExplicitNamespaces, GADTs #-}
{-# LANGUAGE TemplateHaskell, TypeFamilies, TypeOperators         #-}
{-# LANGUAGE UndecidableInstances                                 #-}
-- | Use this module with the following options:
--
--  @{-\# GHC_OPTIONS -fplugin GHC.TypeLits.Symbols.Solver \#-}@
module GHC.TypeLits.Symbols
       (Symbol, type (+++), Head, Tail, ReadInt,
        ToCharList, ViewSymbol, SymbolView, StripPrefix) where
import GHC.TypeLits.Symbols.Internal

import Data.Promotion.TH
import Data.Singletons.Prelude.List (Foldl1, Map)
import Data.Singletons.Prelude.Num
import Data.Singletons.TypeLits

type family Null (s :: Symbol) :: Bool where
  Null "" = 'True
  Null _  = 'False

type Head s = Head' (ViewSymbol s)
type family Head' (v :: SymbolView) :: Symbol where
  Head' ('SymCons x y) = x

type Tail s = Tail' (ViewSymbol s)
type family Tail' (v :: SymbolView) :: Symbol where
  Tail' ('SymCons x y) = y

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

type ReadInt s = Foldl1 ShiftSym0 (Map ReadDigitSym0 (ToCharList s))

type family Recons v where
  Recons 'SymNil = ""
  Recons ('SymCons s t) = s +++ t

type StripPrefix prf s = StripPrefix' (ViewSymbol prf) (ViewSymbol s)
type family StripPrefix' v u :: Maybe Symbol where
  StripPrefix' 'SymNil s = 'Just (Recons s)
  StripPrefix' ('SymCons p ps) ('SymCons p qs) = StripPrefix' (ViewSymbol ps) (ViewSymbol qs)
  StripPrefix' x y = 'Nothing

