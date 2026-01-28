{-# LANGUAGE MultilineStrings #-}
module Preprocess.Builtins.Other where

builtins =
  [ ("flip2", "λ f . λ a1 . λ a2 . f a2 a1"),
    ("curry", "λ f . λ pair . f ( fst pair ) ( snd pair )"),
    ("uncurry", "λ f . λ a1 . λ a2 . f ( a1, a2 )")
  ]
