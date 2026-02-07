{-# LANGUAGE MultilineStrings #-}
module Preprocess.Builtins.Other where

builtins =
  [ ("flip2", "λ f . λ a1 . λ a2 . f a2 a1"),
    ("curry", "λ f . λ pair . f ( fst pair ) ( snd pair )"),
    ("uncurry", "λ f . λ a1 . λ a2 . f ( a1, a2 )"),
    ("null", "in ( inL 𝟙 )"),
    ("cons", "λ item . λ l . in ( inR ( item, next l ) )"),
    ("map", 
        """
        λ f . fix r . λ l . in (
            match ( out l ) { 
                inL unit → inL unit ;
                inR tup → inR ( f ( fst tup ), r ⊙ snd tup )
            } 
        )
        """
    ),
    ("foldr",
        """
        λ x . x
        """
    )
  ]
