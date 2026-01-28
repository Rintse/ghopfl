{-# LANGUAGE MultilineStrings #-}
module Preprocess.Builtins.Conat where

-- Defines the conatural numbers of type
--   μ A . 1 + > A

builtins =
  [ ("co_zero", "in ( inL 𝟙 )"),
    ("co_inf", "fix f . in ( inR f )"),
    ("co_succ", "λ n . in ( inR ( next n ) )"),
    ( "co_isZero",
      """
      λ n . match ( out n ) { 
          inL x → true ;
          inR y → false 
      }
      """
    ),
    ( "co_add",
      """
      fix f . λ n1 . λ n2 . 
          match ( out n1 ) { 
              inL x → n2 ; 
              inR y → in ( inR ( f ⊙ y ⊙ next n2 ) ) 
          }
      """
    ),
    ( "int_to_co",
      """
      fix f . λ n . 
          if ( n = 0 ) then co_zero 
          else in ( inR ( f ⊙ next ( n - 1 ) ) )
      """
    ),
    -- Only for testing, not well typed!
    ( "co_to_int",
      """
      fix f . λ c . 
      match ( out c ) { 
          inL x → 0 ; 
          inR y → 1 + prevI ( f ⊙ y ) 
      }
      """
    )
  ]
