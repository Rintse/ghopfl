
module Semantics.Builtins where

import Syntax.Raw.Abs

builtins :: [Assignment]
builtins =
    [Assign (Ident "exp") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "x") (Pow (DVal 2.71828) (Var (Ident "x")))),Assign (Ident "min") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "x") (Abstr (Lam "\\955") (Ident "y") (Ite (Lt (Var (Ident "x")) (Var (Ident "y"))) (Var (Ident "x")) (Var (Ident "y"))))),Assign (Ident "max") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "x") (Abstr (Lam "\\955") (Ident "y") (Ite (Gt (Var (Ident "x")) (Var (Ident "y"))) (Var (Ident "x")) (Var (Ident "y"))))),Assign (Ident "odd") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "x") (Eq (Mod (Var (Ident "x")) (IVal 2)) (IVal 1))),Assign (Ident "even") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "x") (Eq (Mod (Var (Ident "x")) (IVal 2)) (IVal 0))),Assign (Ident "sqrt") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "x") (Pow (Var (Ident "x")) (DVal 0.5))),Assign (Ident "add") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "x") (Abstr (Lam "\\955") (Ident "y") (Add (Var (Ident "x")) (Var (Ident "y"))))),Assign (Ident "subtract") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "x") (Abstr (Lam "\\955") (Ident "y") (Sub (Var (Ident "x")) (Var (Ident "y"))))),Assign (Ident "divide") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "x") (Abstr (Lam "\\955") (Ident "y") (Div (Var (Ident "x")) (Var (Ident "y"))))),Assign (Ident "multiply") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "x") (Abstr (Lam "\\955") (Ident "y") (Mul (Var (Ident "x")) (Var (Ident "y"))))),Assign (Ident "power") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "x") (Abstr (Lam "\\955") (Ident "y") (Pow (Var (Ident "x")) (Var (Ident "y"))))),Assign (Ident "mod") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "x") (Abstr (Lam "\\955") (Ident "y") (Mod (Var (Ident "x")) (Var (Ident "y"))))),Assign (Ident "xor") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "p") (Abstr (Lam "\\955") (Ident "q") (And (Or (Var (Ident "p")) (Disj "\\8744") (Var (Ident "q"))) (Conj "\\8743") (Not (TNot "\\172") (And (Var (Ident "p")) (Conj "\\8743") (Var (Ident "q"))))))),Assign (Ident "implies") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "p") (Abstr (Lam "\\955") (Ident "q") (Or (Not (TNot "\\172") (Var (Ident "p"))) (Disj "\\8744") (Var (Ident "q"))))),Assign (Ident "not") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "p") (Not (TNot "\\172") (Var (Ident "p"))))] ++
        [Assign (Ident "flip2") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "f") (Abstr (Lam "\\955") (Ident "a1") (Abstr (Lam "\\955") (Ident "a2") (App (App (Var (Ident "f")) (Var (Ident "a2"))) (Var (Ident "a1")))))),Assign (Ident "curry") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "f") (Abstr (Lam "\\955") (Ident "pair") (App (App (Var (Ident "f")) (Fst (Var (Ident "pair")))) (Snd (Var (Ident "pair")))))),Assign (Ident "uncurry") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "f") (Abstr (Lam "\\955") (Ident "a1") (Abstr (Lam "\\955") (Ident "a2") (App (Var (Ident "f")) (Pair (Var (Ident "a1")) (Var (Ident "a2")))))))] ++
        [Assign (Ident "co_zero") (TSub "\\8592") (In (InL (Single (TSingle "\\120793")))),Assign (Ident "co_inf") (TSub "\\8592") (Rec (Ident "f") (In (InR (Var (Ident "f"))))),Assign (Ident "co_succ") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "n") (In (InR (Next (Var (Ident "n")))))),Assign (Ident "co_isZero") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "n") (Match (Out (Var (Ident "n"))) (Ident "x") (TMatch "\\8594") (BVal BTrue) (Ident "y") (TMatch "\\8594") (BVal BFalse))),Assign (Ident "co_add") (TSub "\\8592") (Rec (Ident "f") (Abstr (Lam "\\955") (Ident "n1") (Abstr (Lam "\\955") (Ident "n2") (Match (Out (Var (Ident "n1"))) (Ident "x") (TMatch "\\8594") (Var (Ident "n2")) (Ident "y") (TMatch "\\8594") (In (InR (LApp (LApp (Var (Ident "f")) (TLApp "\\8857") (Var (Ident "y"))) (TLApp "\\8857") (Next (Var (Ident "n2")))))))))),Assign (Ident "int_to_co") (TSub "\\8592") (Rec (Ident "f") (Abstr (Lam "\\955") (Ident "n") (Ite (Eq (Var (Ident "n")) (IVal 0)) (In (InL (Single (TSingle "\\120793")))) (In (InR (LApp (Var (Ident "f")) (TLApp "\\8857") (Next (Sub (Var (Ident "n")) (IVal 1))))))))),Assign (Ident "co_to_int") (TSub "\\8592") (Rec (Ident "f") (Abstr (Lam "\\955") (Ident "c") (Match (Out (Var (Ident "c"))) (Ident "x") (TMatch "\\8594") (IVal 0) (Ident "y") (TMatch "\\8594") (Add (IVal 1) (PrevI (LApp (Var (Ident "f")) (TLApp "\\8857") (Var (Ident "y"))))))))] ++
        [Assign (Ident "partial_now") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "x") (In (InL (Var (Ident "x"))))),Assign (Ident "partial_later") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "x") (In (InR (Var (Ident "x"))))),Assign (Ident "partial_delay") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "x") (In (InR (Next (Var (Ident "x")))))),Assign (Ident "partial_map") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "func") (Rec (Ident "f") (Abstr (Lam "\\955") (Ident "x") (Match (Out (Var (Ident "x"))) (Ident "value") (TMatch "\\8594") (App (Abstr (Lam "\\955") (Ident "x") (In (InL (Var (Ident "x"))))) (App (Var (Ident "func")) (Var (Ident "value")))) (Ident "delay") (TMatch "\\8594") (App (Abstr (Lam "\\955") (Ident "x") (In (InR (Var (Ident "x"))))) (LApp (Var (Ident "f")) (TLApp "\\8857") (Var (Ident "delay")))))))),Assign (Ident "partial_func2_1") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "func") (Abstr (Lam "\\955") (Ident "a") (Rec (Ident "f") (Abstr (Lam "\\955") (Ident "x") (Match (Out (Var (Ident "x"))) (Ident "value") (TMatch "\\8594") (App (Abstr (Lam "\\955") (Ident "x") (In (InL (Var (Ident "x"))))) (App (App (Var (Ident "func")) (Var (Ident "a"))) (Var (Ident "value")))) (Ident "delay") (TMatch "\\8594") (App (Abstr (Lam "\\955") (Ident "x") (In (InR (Var (Ident "x"))))) (LApp (Var (Ident "f")) (TLApp "\\8857") (Var (Ident "delay"))))))))),Assign (Ident "partial_func2") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "func") (Rec (Ident "f") (Abstr (Lam "\\955") (Ident "x1") (Abstr (Lam "\\955") (Ident "x2") (Match (Out (Var (Ident "x1"))) (Ident "value") (TMatch "\\8594") (App (App (App (Abstr (Lam "\\955") (Ident "func") (Abstr (Lam "\\955") (Ident "a") (Rec (Ident "f") (Abstr (Lam "\\955") (Ident "x") (Match (Out (Var (Ident "x"))) (Ident "value") (TMatch "\\8594") (App (Abstr (Lam "\\955") (Ident "x") (In (InL (Var (Ident "x"))))) (App (App (Var (Ident "func")) (Var (Ident "a"))) (Var (Ident "value")))) (Ident "delay") (TMatch "\\8594") (App (Abstr (Lam "\\955") (Ident "x") (In (InR (Var (Ident "x"))))) (LApp (Var (Ident "f")) (TLApp "\\8857") (Var (Ident "delay"))))))))) (Var (Ident "func"))) (Var (Ident "value"))) (Var (Ident "x2"))) (Ident "delay") (TMatch "\\8594") (App (Abstr (Lam "\\955") (Ident "x") (In (InR (Var (Ident "x"))))) (LApp (LApp (Var (Ident "f")) (TLApp "\\8857") (Var (Ident "delay"))) (TLApp "\\8857") (Next (Var (Ident "x2"))))))))))] ++
        [Assign (Ident "rand_idx") (TSub "\\8592") (App (App (Rec (Ident "f") (Abstr (Lam "\\955") (Ident "r") (Abstr (Lam "\\955") (Ident "n") (Abstr (Lam "\\955") (Ident "size") (Ite (Leq (Var (Ident "r")) (TLeq "\\8804") (Mul (Var (Ident "n")) (Div (IVal 1) (Var (Ident "size"))))) (Sub (Var (Ident "n")) (IVal 1)) (PrevI (LApp (LApp (LApp (Var (Ident "f")) (TLApp "\\8857") (Next (Var (Ident "r")))) (TLApp "\\8857") (Next (Add (Var (Ident "n")) (IVal 1)))) (TLApp "\\8857") (Next (Var (Ident "size")))))))))) Rand) (IVal 0))] ++
        [Assign (Ident "l_empty_gl") (TSub "\\8592") (In (InL (Single (TSingle "\\120793")))),Assign (Ident "cons_gl") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "e") (Abstr (Lam "\\955") (Ident "l") (In (InR (Pair (Var (Ident "e")) (Var (Ident "l"))))))),Assign (Ident "head_gl") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "l") (Match (Out (Var (Ident "l"))) (Ident "x") (TMatch "\\8594") (Var (Ident "x")) (Ident "y") (TMatch "\\8594") (Fst (Var (Ident "y"))))),Assign (Ident "tail_gl") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "l") (Match (Out (Var (Ident "l"))) (Ident "x") (TMatch "\\8594") (Var (Ident "x")) (Ident "y") (TMatch "\\8594") (Snd (Var (Ident "y"))))),Assign (Ident "append_gl") (TSub "\\8592") (Rec (Ident "f") (Abstr (Lam "\\955") (Ident "l2") (Abstr (Lam "\\955") (Ident "list") (Match (Out (Var (Ident "list"))) (Ident "end") (TMatch "\\8594") (Var (Ident "l2")) (Ident "lst") (TMatch "\\8594") (App (App (Abstr (Lam "\\955") (Ident "e") (Abstr (Lam "\\955") (Ident "l") (In (InR (Pair (Var (Ident "e")) (Var (Ident "l"))))))) (App (Abstr (Lam "\\955") (Ident "l") (Match (Out (Var (Ident "l"))) (Ident "x") (TMatch "\\8594") (Var (Ident "x")) (Ident "y") (TMatch "\\8594") (Fst (Var (Ident "y"))))) (Var (Ident "list")))) (LApp (LApp (Var (Ident "f")) (TLApp "\\8857") (Next (Var (Ident "l2")))) (TLApp "\\8857") (App (Abstr (Lam "\\955") (Ident "l") (Match (Out (Var (Ident "l"))) (Ident "x") (TMatch "\\8594") (Var (Ident "x")) (Ident "y") (TMatch "\\8594") (Snd (Var (Ident "y"))))) (Var (Ident "list"))))))))),Assign (Ident "take_gl") (TSub "\\8592") (Rec (Ident "f") (Abstr (Lam "\\955") (Ident "n") (Abstr (Lam "\\955") (Ident "list") (Match (Out (Var (Ident "list"))) (Ident "x") (TMatch "\\8594") (Var (Ident "list")) (Ident "y") (TMatch "\\8594") (Ite (Eq (Var (Ident "n")) (IVal 0)) (In (InL (Single (TSingle "\\120793")))) (App (App (Abstr (Lam "\\955") (Ident "e") (Abstr (Lam "\\955") (Ident "l") (In (InR (Pair (Var (Ident "e")) (Var (Ident "l"))))))) (App (Abstr (Lam "\\955") (Ident "l") (Match (Out (Var (Ident "l"))) (Ident "x") (TMatch "\\8594") (Var (Ident "x")) (Ident "y") (TMatch "\\8594") (Fst (Var (Ident "y"))))) (Var (Ident "list")))) (LApp (LApp (Var (Ident "f")) (TLApp "\\8857") (Next (Sub (Var (Ident "n")) (IVal 1)))) (TLApp "\\8857") (App (Abstr (Lam "\\955") (Ident "l") (Match (Out (Var (Ident "l"))) (Ident "x") (TMatch "\\8594") (Var (Ident "x")) (Ident "y") (TMatch "\\8594") (Snd (Var (Ident "y"))))) (Var (Ident "list")))))))))),Assign (Ident "edit_gl") (TSub "\\8592") (Rec (Ident "f") (Abstr (Lam "\\955") (Ident "n") (Abstr (Lam "\\955") (Ident "func") (Abstr (Lam "\\955") (Ident "list") (Match (Out (Var (Ident "list"))) (Ident "x") (TMatch "\\8594") (Var (Ident "list")) (Ident "y") (TMatch "\\8594") (Ite (Eq (Var (Ident "n")) (IVal 0)) (App (App (Abstr (Lam "\\955") (Ident "e") (Abstr (Lam "\\955") (Ident "l") (In (InR (Pair (Var (Ident "e")) (Var (Ident "l"))))))) (App (Var (Ident "func")) (App (Abstr (Lam "\\955") (Ident "l") (Match (Out (Var (Ident "l"))) (Ident "x") (TMatch "\\8594") (Var (Ident "x")) (Ident "y") (TMatch "\\8594") (Fst (Var (Ident "y"))))) (Var (Ident "list"))))) (App (Abstr (Lam "\\955") (Ident "l") (Match (Out (Var (Ident "l"))) (Ident "x") (TMatch "\\8594") (Var (Ident "x")) (Ident "y") (TMatch "\\8594") (Snd (Var (Ident "y"))))) (Var (Ident "list")))) (App (App (Abstr (Lam "\\955") (Ident "e") (Abstr (Lam "\\955") (Ident "l") (In (InR (Pair (Var (Ident "e")) (Var (Ident "l"))))))) (App (Abstr (Lam "\\955") (Ident "l") (Match (Out (Var (Ident "l"))) (Ident "x") (TMatch "\\8594") (Var (Ident "x")) (Ident "y") (TMatch "\\8594") (Fst (Var (Ident "y"))))) (Var (Ident "list")))) (LApp (LApp (LApp (Var (Ident "f")) (TLApp "\\8857") (Next (Sub (Var (Ident "n")) (IVal 1)))) (TLApp "\\8857") (Next (Var (Ident "func")))) (TLApp "\\8857") (App (Abstr (Lam "\\955") (Ident "l") (Match (Out (Var (Ident "l"))) (Ident "x") (TMatch "\\8594") (Var (Ident "x")) (Ident "y") (TMatch "\\8594") (Snd (Var (Ident "y"))))) (Var (Ident "list"))))))))))),Assign (Ident "map_gl") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "func") (Rec (Ident "f") (Abstr (Lam "\\955") (Ident "list") (Match (Out (Var (Ident "list"))) (Ident "end") (TMatch "\\8594") (Var (Ident "list")) (Ident "lst") (TMatch "\\8594") (App (App (Abstr (Lam "\\955") (Ident "e") (Abstr (Lam "\\955") (Ident "l") (In (InR (Pair (Var (Ident "e")) (Var (Ident "l"))))))) (App (Var (Ident "func")) (App (Abstr (Lam "\\955") (Ident "l") (Match (Out (Var (Ident "l"))) (Ident "x") (TMatch "\\8594") (Var (Ident "x")) (Ident "y") (TMatch "\\8594") (Fst (Var (Ident "y"))))) (Var (Ident "list"))))) (LApp (Var (Ident "f")) (TLApp "\\8857") (App (Abstr (Lam "\\955") (Ident "l") (Match (Out (Var (Ident "l"))) (Ident "x") (TMatch "\\8594") (Var (Ident "x")) (Ident "y") (TMatch "\\8594") (Snd (Var (Ident "y"))))) (Var (Ident "list"))))))))),Assign (Ident "limit_gl") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "func") (Abstr (Lam "\\955") (Ident "x") (BoxI (App (Unbox (Var (Ident "func"))) (Unbox (Var (Ident "x"))))))),Assign (Ident "l_empty") (TSub "\\8592") (BoxI (In (InL (Single (TSingle "\\120793"))))),Assign (Ident "cons_cl") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "e") (Abstr (Lam "\\955") (Ident "l") (BoxI (In (InR (Pair (Var (Ident "e")) (Next (Unbox (Var (Ident "l")))))))))),Assign (Ident "head_cl") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "l") (Match (Out (Unbox (Var (Ident "l")))) (Ident "x") (TMatch "\\8594") (Var (Ident "x")) (Ident "y") (TMatch "\\8594") (Fst (Var (Ident "y"))))),Assign (Ident "tail_cl") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "l") (Match (Out (Unbox (Var (Ident "l")))) (Ident "x") (TMatch "\\8594") (Var (Ident "x")) (Ident "y") (TMatch "\\8594") (BoxI (PrevI (Snd (Var (Ident "y"))))))),Assign (Ident "null_cl") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "l") (Match (Out (Unbox (Var (Ident "l")))) (Ident "x") (TMatch "\\8594") (BVal BTrue) (Ident "y") (TMatch "\\8594") (BVal BFalse))),Assign (Ident "l_2nd") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "list") (ListHead (ListTail (Var (Ident "list"))))),Assign (Ident "l_3rd") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "list") (ListHead (ListTail (ListTail (Var (Ident "list")))))),Assign (Ident "l_4th") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "list") (ListHead (ListTail (ListTail (ListTail (Var (Ident "list"))))))),Assign (Ident "co_length") (TSub "\\8592") (Rec (Ident "f") (Abstr (Lam "\\955") (Ident "l") (Match (Out (Unbox (Var (Ident "l")))) (Ident "x") (TMatch "\\8594") (In (InL (Single (TSingle "\\120793")))) (Ident "y") (TMatch "\\8594") (In (InR (LApp (Var (Ident "f")) (TLApp "\\8857") (Next (ListTail (Var (Ident "l")))))))))),Assign (Ident "co_index") (TSub "\\8592") (Rec (Ident "f") (Abstr (Lam "\\955") (Ident "idx") (Abstr (Lam "\\955") (Ident "list") (Match (Out (Var (Ident "idx"))) (Ident "x") (TMatch "\\8594") (ListHead (Var (Ident "list"))) (Ident "y") (TMatch "\\8594") (PrevI (LApp (Var (Ident "f")) (TLApp "\\8857") (Next (Var (Ident "l"))))))))),Assign (Ident "append_cl") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "l1") (Abstr (Lam "\\955") (Ident "l2") (App (App (Abstr (Lam "\\955") (Ident "func") (Abstr (Lam "\\955") (Ident "x") (BoxI (App (Unbox (Var (Ident "func"))) (Unbox (Var (Ident "x"))))))) (BoxI (App (Rec (Ident "f") (Abstr (Lam "\\955") (Ident "l2") (Abstr (Lam "\\955") (Ident "list") (Match (Out (Var (Ident "list"))) (Ident "end") (TMatch "\\8594") (Var (Ident "l2")) (Ident "lst") (TMatch "\\8594") (App (App (Abstr (Lam "\\955") (Ident "e") (Abstr (Lam "\\955") (Ident "l") (In (InR (Pair (Var (Ident "e")) (Var (Ident "l"))))))) (App (Abstr (Lam "\\955") (Ident "l") (Match (Out (Var (Ident "l"))) (Ident "x") (TMatch "\\8594") (Var (Ident "x")) (Ident "y") (TMatch "\\8594") (Fst (Var (Ident "y"))))) (Var (Ident "list")))) (LApp (LApp (Var (Ident "f")) (TLApp "\\8857") (Next (Var (Ident "l2")))) (TLApp "\\8857") (App (Abstr (Lam "\\955") (Ident "l") (Match (Out (Var (Ident "l"))) (Ident "x") (TMatch "\\8594") (Var (Ident "x")) (Ident "y") (TMatch "\\8594") (Snd (Var (Ident "y"))))) (Var (Ident "list"))))))))) (Unbox (Var (Ident "l2")))))) (Var (Ident "l1"))))),Assign (Ident "take_cl") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "n") (App (Abstr (Lam "\\955") (Ident "func") (Abstr (Lam "\\955") (Ident "x") (BoxI (App (Unbox (Var (Ident "func"))) (Unbox (Var (Ident "x"))))))) (BoxI (App (Rec (Ident "f") (Abstr (Lam "\\955") (Ident "n") (Abstr (Lam "\\955") (Ident "list") (Match (Out (Var (Ident "list"))) (Ident "x") (TMatch "\\8594") (Var (Ident "list")) (Ident "y") (TMatch "\\8594") (Ite (Eq (Var (Ident "n")) (IVal 0)) (In (InL (Single (TSingle "\\120793")))) (App (App (Abstr (Lam "\\955") (Ident "e") (Abstr (Lam "\\955") (Ident "l") (In (InR (Pair (Var (Ident "e")) (Var (Ident "l"))))))) (App (Abstr (Lam "\\955") (Ident "l") (Match (Out (Var (Ident "l"))) (Ident "x") (TMatch "\\8594") (Var (Ident "x")) (Ident "y") (TMatch "\\8594") (Fst (Var (Ident "y"))))) (Var (Ident "list")))) (LApp (LApp (Var (Ident "f")) (TLApp "\\8857") (Next (Sub (Var (Ident "n")) (IVal 1)))) (TLApp "\\8857") (App (Abstr (Lam "\\955") (Ident "l") (Match (Out (Var (Ident "l"))) (Ident "x") (TMatch "\\8594") (Var (Ident "x")) (Ident "y") (TMatch "\\8594") (Snd (Var (Ident "y"))))) (Var (Ident "list")))))))))) (Var (Ident "n")))))),Assign (Ident "edit_cl") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "n") (Abstr (Lam "\\955") (Ident "func") (App (Abstr (Lam "\\955") (Ident "func") (Abstr (Lam "\\955") (Ident "x") (BoxI (App (Unbox (Var (Ident "func"))) (Unbox (Var (Ident "x"))))))) (BoxI (App (App (Rec (Ident "f") (Abstr (Lam "\\955") (Ident "n") (Abstr (Lam "\\955") (Ident "func") (Abstr (Lam "\\955") (Ident "list") (Match (Out (Var (Ident "list"))) (Ident "x") (TMatch "\\8594") (Var (Ident "list")) (Ident "y") (TMatch "\\8594") (Ite (Eq (Var (Ident "n")) (IVal 0)) (App (App (Abstr (Lam "\\955") (Ident "e") (Abstr (Lam "\\955") (Ident "l") (In (InR (Pair (Var (Ident "e")) (Var (Ident "l"))))))) (App (Var (Ident "func")) (App (Abstr (Lam "\\955") (Ident "l") (Match (Out (Var (Ident "l"))) (Ident "x") (TMatch "\\8594") (Var (Ident "x")) (Ident "y") (TMatch "\\8594") (Fst (Var (Ident "y"))))) (Var (Ident "list"))))) (App (Abstr (Lam "\\955") (Ident "l") (Match (Out (Var (Ident "l"))) (Ident "x") (TMatch "\\8594") (Var (Ident "x")) (Ident "y") (TMatch "\\8594") (Snd (Var (Ident "y"))))) (Var (Ident "list")))) (App (App (Abstr (Lam "\\955") (Ident "e") (Abstr (Lam "\\955") (Ident "l") (In (InR (Pair (Var (Ident "e")) (Var (Ident "l"))))))) (App (Abstr (Lam "\\955") (Ident "l") (Match (Out (Var (Ident "l"))) (Ident "x") (TMatch "\\8594") (Var (Ident "x")) (Ident "y") (TMatch "\\8594") (Fst (Var (Ident "y"))))) (Var (Ident "list")))) (LApp (LApp (LApp (Var (Ident "f")) (TLApp "\\8857") (Next (Sub (Var (Ident "n")) (IVal 1)))) (TLApp "\\8857") (Next (Var (Ident "func")))) (TLApp "\\8857") (App (Abstr (Lam "\\955") (Ident "l") (Match (Out (Var (Ident "l"))) (Ident "x") (TMatch "\\8594") (Var (Ident "x")) (Ident "y") (TMatch "\\8594") (Snd (Var (Ident "y"))))) (Var (Ident "list"))))))))))) (Var (Ident "n"))) (Var (Ident "func"))))))),Assign (Ident "map_cl") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "func") (App (Abstr (Lam "\\955") (Ident "func") (Abstr (Lam "\\955") (Ident "x") (BoxI (App (Unbox (Var (Ident "func"))) (Unbox (Var (Ident "x"))))))) (BoxI (App (Abstr (Lam "\\955") (Ident "func") (Rec (Ident "f") (Abstr (Lam "\\955") (Ident "list") (Match (Out (Var (Ident "list"))) (Ident "end") (TMatch "\\8594") (Var (Ident "list")) (Ident "lst") (TMatch "\\8594") (App (App (Abstr (Lam "\\955") (Ident "e") (Abstr (Lam "\\955") (Ident "l") (In (InR (Pair (Var (Ident "e")) (Var (Ident "l"))))))) (App (Var (Ident "func")) (App (Abstr (Lam "\\955") (Ident "l") (Match (Out (Var (Ident "l"))) (Ident "x") (TMatch "\\8594") (Var (Ident "x")) (Ident "y") (TMatch "\\8594") (Fst (Var (Ident "y"))))) (Var (Ident "list"))))) (LApp (Var (Ident "f")) (TLApp "\\8857") (App (Abstr (Lam "\\955") (Ident "l") (Match (Out (Var (Ident "l"))) (Ident "x") (TMatch "\\8594") (Var (Ident "x")) (Ident "y") (TMatch "\\8594") (Snd (Var (Ident "y"))))) (Var (Ident "list"))))))))) (Var (Ident "func")))))),Assign (Ident "foldl_4") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "func") (Abstr (Lam "\\955") (Ident "val") (Abstr (Lam "\\955") (Ident "list") (App (App (Var (Ident "func")) (App (App (Var (Ident "func")) (App (App (Var (Ident "func")) (App (App (Var (Ident "func")) (Var (Ident "val"))) (ListHead (Var (Ident "list"))))) (App (Abstr (Lam "\\955") (Ident "list") (ListHead (ListTail (Var (Ident "list"))))) (Var (Ident "list"))))) (App (Abstr (Lam "\\955") (Ident "list") (ListHead (ListTail (ListTail (Var (Ident "list")))))) (Var (Ident "list"))))) (App (Abstr (Lam "\\955") (Ident "list") (ListHead (ListTail (ListTail (ListTail (Var (Ident "list"))))))) (Var (Ident "list"))))))),Assign (Ident "index_cheat") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "idx") (Abstr (Lam "\\955") (Ident "list") (Ite (Eq (Var (Ident "idx")) (IVal 0)) (ListHead (Var (Ident "list"))) (Ite (Eq (Var (Ident "idx")) (IVal 1)) (App (Abstr (Lam "\\955") (Ident "list") (ListHead (ListTail (Var (Ident "list"))))) (Var (Ident "list"))) (Ite (Eq (Var (Ident "idx")) (IVal 2)) (App (Abstr (Lam "\\955") (Ident "list") (ListHead (ListTail (ListTail (Var (Ident "list")))))) (Var (Ident "list"))) (App (Abstr (Lam "\\955") (Ident "list") (ListHead (ListTail (ListTail (ListTail (Var (Ident "list"))))))) (Var (Ident "list")))))))),Assign (Ident "length_cl") (TSub "\\8592") (Rec (Ident "f") (Abstr (Lam "\\955") (Ident "l") (Match (Out (Unbox (Var (Ident "l")))) (Ident "x") (TMatch "\\8594") (IVal 0) (Ident "y") (TMatch "\\8594") (Add (IVal 1) (PrevI (LApp (Var (Ident "f")) (TLApp "\\8857") (Next (ListTail (Var (Ident "l")))))))))),Assign (Ident "index_cl") (TSub "\\8592") (Rec (Ident "f") (Abstr (Lam "\\955") (Ident "idx") (Abstr (Lam "\\955") (Ident "list") (Ite (Eq (Var (Ident "idx")) (IVal 0)) (ListHead (Var (Ident "list"))) (PrevI (LApp (LApp (Var (Ident "f")) (TLApp "\\8857") (Next (Sub (Var (Ident "idx")) (IVal 1)))) (TLApp "\\8857") (Next (ListTail (Var (Ident "list")))))))))),Assign (Ident "foldl_cl") (TSub "\\8592") (Rec (Ident "f") (Abstr (Lam "\\955") (Ident "func") (Abstr (Lam "\\955") (Ident "val") (Abstr (Lam "\\955") (Ident "list") (Match (Out (Unbox (Var (Ident "list")))) (Ident "end") (TMatch "\\8594") (Var (Ident "val")) (Ident "lst") (TMatch "\\8594") (PrevI (LApp (LApp (LApp (Var (Ident "f")) (TLApp "\\8857") (Next (Var (Ident "func")))) (TLApp "\\8857") (Next (App (App (Var (Ident "func")) (ListHead (Var (Ident "list")))) (Var (Ident "val"))))) (TLApp "\\8857") (Next (ListTail (Var (Ident "list"))))))))))),Assign (Ident "foldr_cl") (TSub "\\8592") (Rec (Ident "f") (Abstr (Lam "\\955") (Ident "func") (Abstr (Lam "\\955") (Ident "val") (Abstr (Lam "\\955") (Ident "list") (Match (Out (Unbox (Var (Ident "list")))) (Ident "end") (TMatch "\\8594") (Var (Ident "val")) (Ident "lst") (TMatch "\\8594") (App (App (Var (Ident "func")) (PrevI (LApp (LApp (LApp (Var (Ident "f")) (TLApp "\\8857") (Next (Var (Ident "func")))) (TLApp "\\8857") (Next (Var (Ident "val")))) (TLApp "\\8857") (Next (ListTail (Var (Ident "l"))))))) (ListHead (Var (Ident "l")))))))))] ++
        [Assign (Ident "s_cons_g") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "e") (Abstr (Lam "\\955") (Ident "s") (In (Pair (Var (Ident "e")) (Next (Var (Ident "s"))))))),Assign (Ident "s_cons_g_n") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "e") (Abstr (Lam "\\955") (Ident "s") (In (Pair (Var (Ident "e")) (Var (Ident "s")))))),Assign (Ident "s_head_g") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "s") (Fst (Out (Var (Ident "s"))))),Assign (Ident "s_tail_g") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "s") (Snd (Out (Var (Ident "s"))))),Assign (Ident "s_idx_g") (TSub "\\8592") (Rec (Ident "f") (Abstr (Lam "\\955") (Ident "n") (Abstr (Lam "\\955") (Ident "s") (Ite (Eq (Var (Ident "n")) (IVal 0)) (In (InL (App (Abstr (Lam "\\955") (Ident "s") (Fst (Out (Var (Ident "s"))))) (Var (Ident "s"))))) (In (InR (LApp (LApp (Var (Ident "f")) (TLApp "\\8857") (Next (Sub (Var (Ident "n")) (IVal 1)))) (TLApp "\\8857") (App (Abstr (Lam "\\955") (Ident "s") (Snd (Out (Var (Ident "s"))))) (Var (Ident "s")))))))))),Assign (Ident "s_2nd_g") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "s") (LApp (Next (Abstr (Lam "\\955") (Ident "s") (Fst (Out (Var (Ident "s")))))) (TLApp "\\8857") (App (Abstr (Lam "\\955") (Ident "s") (Snd (Out (Var (Ident "s"))))) (Var (Ident "s"))))),Assign (Ident "s_3rd_g") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "s") (LApp (Next (Abstr (Lam "\\955") (Ident "s") (LApp (Next (Abstr (Lam "\\955") (Ident "s") (Fst (Out (Var (Ident "s")))))) (TLApp "\\8857") (App (Abstr (Lam "\\955") (Ident "s") (Snd (Out (Var (Ident "s"))))) (Var (Ident "s")))))) (TLApp "\\8857") (App (Abstr (Lam "\\955") (Ident "s") (Snd (Out (Var (Ident "s"))))) (Var (Ident "s"))))),Assign (Ident "s_4th_g") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "s") (LApp (Next (Abstr (Lam "\\955") (Ident "s") (LApp (Next (Abstr (Lam "\\955") (Ident "s") (LApp (Next (Abstr (Lam "\\955") (Ident "s") (Fst (Out (Var (Ident "s")))))) (TLApp "\\8857") (App (Abstr (Lam "\\955") (Ident "s") (Snd (Out (Var (Ident "s"))))) (Var (Ident "s")))))) (TLApp "\\8857") (App (Abstr (Lam "\\955") (Ident "s") (Snd (Out (Var (Ident "s"))))) (Var (Ident "s")))))) (TLApp "\\8857") (App (Abstr (Lam "\\955") (Ident "s") (Snd (Out (Var (Ident "s"))))) (Var (Ident "s"))))),Assign (Ident "s_map_g") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "func") (Rec (Ident "f") (Abstr (Lam "\\955") (Ident "s") (App (App (Abstr (Lam "\\955") (Ident "e") (Abstr (Lam "\\955") (Ident "s") (In (Pair (Var (Ident "e")) (Var (Ident "s")))))) (App (Var (Ident "func")) (App (Abstr (Lam "\\955") (Ident "s") (Fst (Out (Var (Ident "s"))))) (Var (Ident "s"))))) (LApp (Var (Ident "f")) (TLApp "\\8857") (App (Abstr (Lam "\\955") (Ident "s") (Snd (Out (Var (Ident "s"))))) (Var (Ident "s")))))))),Assign (Ident "s_head_c") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "s") (App (Abstr (Lam "\\955") (Ident "s") (Fst (Out (Var (Ident "s"))))) (Unbox (Var (Ident "s"))))),Assign (Ident "s_tail_c") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "s") (BoxI (PrevI (App (Abstr (Lam "\\955") (Ident "s") (Snd (Out (Var (Ident "s"))))) (Unbox (Var (Ident "s"))))))),Assign (Ident "s_idx_c") (TSub "\\8592") (Rec (Ident "f") (Abstr (Lam "\\955") (Ident "n") (Abstr (Lam "\\955") (Ident "s") (Ite (Eq (Var (Ident "n")) (IVal 0)) (In (InL (App (Abstr (Lam "\\955") (Ident "s") (App (Abstr (Lam "\\955") (Ident "s") (Fst (Out (Var (Ident "s"))))) (Unbox (Var (Ident "s"))))) (Var (Ident "s"))))) (In (InR (LApp (LApp (Var (Ident "f")) (TLApp "\\8857") (Next (Sub (Var (Ident "n")) (IVal 1)))) (TLApp "\\8857") (Next (App (Abstr (Lam "\\955") (Ident "s") (BoxI (PrevI (App (Abstr (Lam "\\955") (Ident "s") (Snd (Out (Var (Ident "s"))))) (Unbox (Var (Ident "s"))))))) (Var (Ident "s"))))))))))),Assign (Ident "s_limit_c") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "func") (Abstr (Lam "\\955") (Ident "x") (BoxI (App (Unbox (Var (Ident "func"))) (Unbox (Var (Ident "x"))))))),Assign (Ident "s_map_c") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "f") (App (Abstr (Lam "\\955") (Ident "func") (Abstr (Lam "\\955") (Ident "x") (BoxI (App (Unbox (Var (Ident "func"))) (Unbox (Var (Ident "x"))))))) (BoxI (App (Abstr (Lam "\\955") (Ident "func") (Rec (Ident "f") (Abstr (Lam "\\955") (Ident "s") (App (App (Abstr (Lam "\\955") (Ident "e") (Abstr (Lam "\\955") (Ident "s") (In (Pair (Var (Ident "e")) (Var (Ident "s")))))) (App (Var (Ident "func")) (App (Abstr (Lam "\\955") (Ident "s") (Fst (Out (Var (Ident "s"))))) (Var (Ident "s"))))) (LApp (Var (Ident "f")) (TLApp "\\8857") (App (Abstr (Lam "\\955") (Ident "s") (Snd (Out (Var (Ident "s"))))) (Var (Ident "s")))))))) (Var (Ident "f"))))))]