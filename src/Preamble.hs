module Preamble
  ( module Protolude
  , (.!)
  , (.!!)
  , (.!!!)
  , (.!!!!)
  ) where


import Protolude

(.!) :: (b -> c) -> (a -> b) -> a -> c
(.!) = (.)
infixl 8 .!

(.!!) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.!!) = (.) . (.)
infixl 8 .!!

(.!!!) :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
(.!!!) = (.) . (.) . (.)
infixl 8 .!!!

(.!!!!) :: (e -> f) -> (a -> b -> c -> d -> e) -> a -> b -> c -> d -> f
(.!!!!) = (.) . (.) . (.) . (.)
infixl 8 .!!!!

