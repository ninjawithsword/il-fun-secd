{--------------------------------------
  Fun: a minimal functional language
  -------------------------------------
 
  This module contains some example programs.
  Pedro Vasconcelos, 2008--2009.
-}
module Examples where
import Fun

-- a simple computation
ex1 = (Const 42 :+ Const 23) :* Const 5

-- the identity function
ex2 = Lambda "x" (Var "x")

-- the sucessor function
ex3 = Lambda "x" (Var "x" :+ Const 1)

-- one function between two integers
ex4 = Lambda "x" 
      (Lambda "y"
       (IfZero (Var "x" :- Var "y") 
        (Var "y") (Var "x")))
                       
-- an example that builds a closure with free vars
ex5 = Let "x" (Const 42) (Lambda "y" (Var "x" :+ Var "y"))

ex5b = Let "x" (Const 23) (Lambda "y" (Var "x" :+ Var "y"))

-- a recursive function (factorial)
ex6 = Fix 
      (Lambda "f" 
       (Lambda "n"
        (IfZero (Var "n")
         (Const 1)
         ((App (Var "f") (Var "n" :- Const 1)) :* Var "n")
        )))


-- compute the factorial of 10
ex7 = App ex6 (Const 10)


-- factorial of a negative number (diverges)
ex9 = App ex6  (Const (-1))

{-
-- recursive sum 1^2+2^2+...+n^2
ex8 = Fix 
      (Lambda "f"
       (Lambda "n"
         (IfThenElse (Var "n" :== Const 0)
          (Const 0)
           ((Var "n" :* Var "n") :+ 
            App (Var "f") (Var "n" :- Const 1)))))


-- divergent computation (\x.x x) (\x.x x)
omega = let t = Lambda "x" (App (Var "x") (Var "x"))
        in App t t

-- exame normal
exame1 = Let "f" (Let "y" (Const 3) (Lambda "x" (Var "x" :+ Var "y")))
                  (App (Var "f") (Const 5))
         

-- exame recurso
exame2 = Let "f" (Lambda "x" (Lambda "y" (Var "x" :+ Var "y")))
         (App (App (Var "f") (Const 2)) (Const 3))


-- exame especial
ex9 = Let "f" 
      (Lambda "x" ((Const 3 :* Var "x") :+ Const 1))
      (App (Var "f")  (Const 5))

-}