{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, LiberalTypeSynonyms        #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables, TemplateHaskell               #-}
{-# LANGUAGE TypeApplications, TypeFamilies, TypeFamilyDependencies         #-}
{-# LANGUAGE TypeInType, TypeOperators, UndecidableInstances, UnicodeSyntax #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Symbols.Solver #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
-- {-# OPTIONS_GHC -ddump-tc-trace #-}
{-# OPTIONS_GHC -O0 #-} -- I don't know why optimization prevents this program from compiling....
module Main where
import Data.Kind
import Data.Singletons.Prelude
import GHC.TypeLits
import GHC.TypeLits.Symbols
import GHC.TypeLits.Symbols.Internal
import System.IO

-- Type-safe printf.
data Format = Lit Symbol | Str | Shown (★)

data instance Sing (x ∷ Format) where
  SLit   ∷ KnownSymbol s ⇒ Sing s → Sing ('Lit s)
  SStr   ∷ Sing 'Str
  SShown ∷ Show a ⇒ Sing ('Shown a)

instance (KnownSymbol s) ⇒ SingI ('Lit s) where
  sing = SLit sing

instance SingI 'Str where
  sing = SStr

instance (Show a) ⇒ SingI ('Shown a) where
  sing = SShown

newtype Showed a = Showed { runShowed :: a }

type family Printf (fmt ∷ [Format])  where
  Printf '[] = String
  Printf ('Lit s ': fmt) = Printf fmt
  Printf ('Str   ': fmt) = String → Printf fmt
  Printf ('Shown a ': fmt) =  a → Printf fmt

printf' ∷ ∀ fmt. [String] → Sing fmt → Printf fmt
printf' ss SNil = concat $ reverse ss
printf' ss (SCons elt elts) = case elt of
  SLit s → printf' (symbolVal s : ss) elts
  SStr → \ s → printf' (s : ss) elts
  SShown → \x → printf' (show x : ss) elts

printf_ ∷ Sing fmt → Printf fmt
printf_ = printf' []

data ParseState = Percent
                | Normal Symbol

type family ParsePrintf' (s ∷ ParseState) (a ∷ SymbolView) ∷ [Format] where
  ParsePrintf' 'Percent       'SymNil         = '[ 'Lit "%" ]
  ParsePrintf' 'Percent      ('SymCons "%" a) = ParsePrintf' ('Normal "%") (ViewSymbol a)
  ParsePrintf' 'Percent      ('SymCons "s" a) = 'Str ': ParsePrintf' ('Normal "") (ViewSymbol a)
  -- ParsePrintf' 'Percent      ('SymCons "S" a) = 'Shown Any ': ParsePrintf' ('Normal "") (ViewSymbol a)
  ParsePrintf' ('Normal str) ('SymCons c "")  = '[ 'Lit (str +++ c) ]
  ParsePrintf' ('Normal "")  ('SymCons "%" a) = ParsePrintf' 'Percent (ViewSymbol a)
  ParsePrintf' ('Normal str) ('SymCons "%" a) = 'Lit str ': ParsePrintf' 'Percent (ViewSymbol a)
  ParsePrintf' ('Normal str) ('SymCons c a)   = ParsePrintf' ('Normal (str +++ c)) (ViewSymbol a)
  ParsePrintf' ('Normal "")   'SymNil         = '[]
  ParsePrintf' ('Normal str)  'SymNil         = '[ 'Lit str ]

type ParsePrintf sym = ParsePrintf' ('Normal "") (ViewSymbol sym)

printf ∷ ∀ sym. (SingI (ParsePrintf sym)) ⇒ Sing sym → Printf (ParsePrintf sym)
printf _ = printf' @(ParsePrintf sym) [] sing

main ∷ IO ()
main = do
  putStr "put your name: "
  hFlush stdout
  putStrLn . printf @"Hello, %s!" sing =<< getLine
