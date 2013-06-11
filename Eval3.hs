{-
  Call-by-value operational evaluator for Fun
  (aka "big-step" or "natural semantics")

  third version: continuation-passing style (CPS)
  Pedro Vasconcelos, 2008
-}
module Eval3 where
import Fun                 -- language abstract syntax

-- environments: associations of identifiers to values
type Env = [(Ident,Value)]

-- values are either integers or closures
data Value = Prim Int
           | Closure Term Env  -- term is always a lambda abstraction
             deriving (Eq,Show)

type Cont = Value -> Value   -- continuations

-- evaluate a term under an environment and continuation
-- the continuation k specifies what should be done with the result
eval3 :: Term -> Env -> Cont -> Value
eval3 (Const n) env k = k (Prim n)
eval3 (Var x)   env k
    = case lookup x env of 
        Just v -> k v
        Nothing -> error ("free variable: "++x)

eval3 (Lambda x e) env k 
  = k (Closure (Lambda x e) env)

eval3 (App e1 e2) env k 
  = eval3 e1 env $ \v1 ->
    eval3 e2 env $ \v2 ->
    apply v1 v2 k 

eval3 (IfZero e1 e2 e3) env k
  = eval3 e1 env $ \v1 -> 
    case v1 of Prim 0 -> eval3 e2 env k
               Prim _ -> eval3 e3 env k

eval3 (e1 :+ e2) env k 
  = eval3 e1 env $ \v1-> 
    eval3 e2 env $ \v2-> 
    k $ primop (+) v1 v2 

eval3 (e1 :- e2) env k
  = eval3 e1 env $ \v1-> 
    eval3 e2 env $ \v2-> 
    k $ primop (-) v1 v2

eval3 (e1 :* e2) env k 
  = eval3 e1 env $ \v1-> 
    eval3 e2 env $ \v2-> 
    k $ primop (*) v1 v2 

eval3 (Let x e1 e2) env k 
  = eval3 (App (Lambda x e2) e1) env k

-- "knot-tying" fixed point combinator:
-- construct a closure with a cyclic environment 
eval3 (Fix (Lambda f e)) env k
  = let v = Closure e env'
        env' = (f,v):env
    in k v

-- perform a beta-reduction
apply :: Value -> Value -> Cont -> Value
apply (Closure (Lambda x e) env) v k = eval3 e ((x,v):env) k
apply _ _ _ = error "apply: non-closure argument"


-- apply a primitive operator
primop :: (Int -> Int -> Int) -> Value -> Value -> Value
primop op (Prim v1) (Prim v2) = Prim (v1`op`v2)
primop _ _ _ = error "primop: non-integer argument"

-- end-of-file --
