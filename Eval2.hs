{-
  ------------------------------------
  Fun: a minimal functional language
  ------------------------------------
  Call-by-value operational evaluator for Fun
  (aka "big-step" or "natural semantics")

  Second version, using environments instead of substitutions
  Pedro Vasconcelos, 2008--2012
-}
module Eval2 where
import Fun 

-- environments: association lists of identifiers to values
type Env = [(Ident,Value)]

-- values (integers or closures)
data Value = Prim Int
           | Closure Term Env  -- term is always a lambda
             deriving (Eq,Show)

-- evaluate a term under an environment
eval2 :: Term -> Env -> Value
eval2 (Const n) env = Prim n
eval2 (Var x)   env 
    = case lookup x env of 
        Just v -> v
        Nothing -> error ("eval2: free variable " ++ show x)

eval2 (Lambda x e) env = Closure (Lambda x e) env

eval2 (App e1 e2) env = apply (eval2 e1 env) $! (eval2 e2 env)

eval2 (IfZero e1 e2 e3) env
  = case eval2 e1 env of 
    Prim 0 -> eval2 e2 env
    Prim _ -> eval2 e3 env

eval2 (Let x e1 e2) env = eval2 (App (Lambda x e2) e1) env

-- "knot-tying" fixed point combinator
-- construct a closure with a cyclic environment 
eval2 (Fix (Lambda f e)) env 
    = let v = Closure e env'
          env' = (f,v):env
      in v

-- primitive operators
eval2 (e1 :+ e2) env = primop (+) (eval2 e1 env) (eval2 e2 env)
eval2 (e1 :- e2) env = primop (-) (eval2 e1 env) (eval2 e2 env)
eval2 (e1 :* e2) env = primop (*) (eval2 e1 env) (eval2 e2 env)

-- auxiliary functions

-- perform a beta-reduction
apply :: Value -> Value -> Value
apply (Closure (Lambda x e) env) v = eval2 e ((x,v):env)
apply _ _ = error "apply: non-closure argument"

-- apply a primitive operator
primop :: (Int -> Int -> Int) -> Value -> Value -> Value
primop op (Prim v1) (Prim v2) = Prim (v1`op`v2)
primop _ _ _ = error "primop: non-integer argument"

-- end of file --
