{-# LANGUAGE DataKinds, GADTs, TypeOperators #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Symbols.Solver #-}
module Main where
import Data.Singletons.Prelude
import Data.Singletons.Prelude.Maybe
import GHC.TypeLits.Symbols
import GHC.TypeLits.Symbols.Internal

a :: (ReadInt "123" ~ 123) => ()
a = ()

caster :: (ReadInt (FromJust (StripPrefix "x_" "x_2")) ~ 2) => ()
caster = ()

main :: IO ()
main = return ()
