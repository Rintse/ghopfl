{-# LANGUAGE MultilineStrings #-}
module Preprocess.Builtins.Lists where

builtins =
  [ ("null", "in ( inL 𝟙 )"),
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
