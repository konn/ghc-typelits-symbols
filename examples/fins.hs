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
import Data.Proxy
import Data.Reflection (reifyNat)
import Data.Type.Equality
import Unsafe.Coerce

type n < m = (n + 1) <= m

data Fin (n :: Nat) where
  Fin :: (KnownNat n, n < m) => SNat n -> Fin m

instance Show (Fin n) where
  showsPrec d (Fin sn) = showsPrec d $ natVal sn

instance (ReadInt (FromJust (StripPrefix "o" lab)) < n, KnownSymbol lab)
      => IsLabel lab (Fin n) where
  fromLabel pxy = reifyNat (read $ tail $ symbolVal' pxy) $ \(Proxy :: Proxy k) ->
    case unsafeCoerce Refl :: (k + 1 <=? n) :~: 'True of -- It's safe! trust me.
      Refl -> Fin (sing :: SNat k)

main :: IO ()
main = do
  print (#o1 :: Fin 4)
  -- print (#o5 :: Fin 2) -- Won't compile!
