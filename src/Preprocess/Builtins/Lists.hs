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
    ),
    ("null_b", "in ( inL 𝟙 )"),
    -- TODO: this does not work
    ("cons_b", "λ item . λ l . boxI ( in ( inR ( item, next ( unbox l ) ) ) )")
  ]
