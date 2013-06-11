{-------------------------------------
  Fun: a minimal functional language
  ------------------------------------
  Call-by-value operational semantics for Fun
  (also called "big-step semantics" or "natural semantics")

  First version using explicit substitutions.
  Pedro Vasconcelos, 2008--2009
-}
module Eval1 where
import Fun

type Value = Term   -- values are just subsets of terms

eval1 :: Term -> Value
eval1 (Const n)    = Const n
eval1 (Var x)      = error ("eval1: free variable " ++ show x)
eval1 (Lambda x e) = Lambda x e
eval1 (App e1 e2)  = apply (eval1 e1) $! (eval1 e2)
-- f $! v is strict application of f to v 
-- this is required otherise eval inherits 
-- the lazy evaluation of Haskell

eval1 (IfZero e1 e2 e3)
  = case eval1 e1 of
      Const 0 -> eval1 e2
      Const _ -> eval1 e3

eval1 (Let x e1 e2) = eval1 (App (Lambda x e2) e1)

-- unfold the fixpoint combinator "one step"
-- fix (\f x->e) => \x -> e[(fix (\f x->e))/f]
eval1 (Fix (Lambda f (Lambda x e))) 
       = Lambda x (subst e f (Fix (Lambda f (Lambda x e))))

-- primitive operations
eval1 (e1 :+ e2) = primop (+) (eval1 e1) (eval1 e2)
eval1 (e1 :- e2) = primop (-) (eval1 e1) (eval1 e2)
eval1 (e1 :* e2) = primop (*) (eval1 e1) (eval1 e2)

-- auxiliary functions
-- perform beta-reduction 
apply :: Term -> Term -> Term
apply (Lambda x e) v = eval1 (subst e x v) 
apply _  _ = error "apply: non-lambda argument"

-- primitive binary operation on integers
primop :: (Int -> Int -> Int) -> Term -> Term -> Term
primop op (Const v1) (Const v2) = Const (v1`op`v2)
primop _ _ _ = error "primop: non-integer argument"

-- auxiliary function to perform substitutions
subst :: Term -> Ident -> Value -> Term
subst (Const n) x t = Const n

subst (Var y) x t 
    | x==y      = t
    | otherwise = Var y

-- no need for alpha-conversion here because we only 
-- substitute terms with no free variables
subst (Lambda y e) x t
    | x==y      = Lambda y e
    | otherwise = Lambda y (subst e x t)

subst (App e1 e2) x t
    = App (subst e1 x t) (subst e2 x t)

subst (e1 :+ e2) x t
    = subst e1 x t :+ subst e2 x t

subst (e1 :- e2) x t
    = subst e1 x t :- subst e2 x t

subst (e1 :* e2) x t
    = subst e1 x t :* subst e2 x t

subst (IfZero e1 e2 e3) x t
    = IfZero (subst e1 x t) (subst e2 x t) (subst e3 x t)

subst (Fix e) x t = Fix (subst e x t)

subst (Let y e1 e2) x t
    | x==y      = Let y (subst e1 x t) e2
    | otherwise = Let y (subst e1 x t) (subst e2 x t)


-- end of file --
