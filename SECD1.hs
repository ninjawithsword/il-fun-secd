{- -----------------------------------
   Fun: a minimal functional language
   -----------------------------------
   A compiler and interpreter for a SECD-like virtual machine.
   
   Pedro Vasconcelos, 2008--2011.
 -}
module SECD1 where
import Fun
import Data.List (elemIndex)
import Data.Map (Map)
import qualified Data.Map as Map

-----------------------------------------------------------------
-- SECD machine definitions
-----------------------------------------------------------------

-- pseudo instructions 
data Instr = HALT            -- finished
           | LDC Int         -- load constant
           | LD Int          -- load variable
           | ADD             -- addition
           | SUB             -- subtraction
           | MUL             -- multiplication
           | SEL [Instr] [Instr] -- select zero/non-zero
           | JOIN            -- close branch
           | LDF [Instr]     -- load a closure
           | LDRF [Instr]    -- load a recursive closure
           | AP              -- apply
           | RTN             -- return 
             deriving Show

-- a code block (list of instructions)
type Code = [Instr]


-- closure: pairs of code, environment
type Closure = (Code, Env)

-- closure addresses
type Addr = Int

-- store for closures
type Store = Map Addr Closure

-- get the next available address
nextAddr :: Store -> Addr
nextAddr store = 1 + Map.size store

-- a value of the SECD machine is either
-- a primitive integer or the address of a closure
data Value = I Int
           | A Addr
             deriving (Eq,Show)

-- the SECD machine components
type Stack = [Value]

type Env   = [Value]

type Dump  = [(Stack,Env,Code)]

-- the SECD machine configuration
type Conf  = (Stack, Env, Code, Dump, Store)


-- execute a single SECD instruction
execute :: Conf -> Conf

execute (stack, env, LDC n:code, dump, store) 
    = (I n:stack, env, code, dump, store)

execute (I v2:I v1:stack, env, ADD:code, dump, store)
    = (I (v1+v2):stack, env, code, dump, store)

execute (I v2:I v1:stack, env, SUB:code, dump, store)
    = (I (v1-v2):stack, env, code, dump, store)

execute (I v2:I v1:stack, env, MUL:code, dump, store)
    = (I (v1*v2):stack, env, code, dump, store)

execute (stack, env, LD i:code, dump, store)
    = let v = env!!i
      in (v:stack, env, code, dump, store)

execute (stack, env, LDF code':code, dump, store)
    = let addr = nextAddr store
          store'= Map.insert addr (code',env) store
      in (A addr:stack, env, code, dump, store')

execute (stack, env, LDRF code':code, dump, store)
    = let addr= nextAddr store
          store'=Map.insert addr (code', A addr:env) store
      in (A addr:stack, env, code, dump, store')


execute (arg:A addr:stack, env, AP:code, dump, store)
    = let Just (code',env')= Map.lookup addr store
      in ([], arg:env', code', (stack,env,code):dump, store)

execute (v:stack, env, RTN:code, (stack',env',code'):dump, store)
    = (v:stack', env', code', dump, store)


execute (I n:stack, env, (SEL code1 code2):code, dump,store)
    | n==0      = (stack, env, code1, ([],[],code):dump, store)
    | otherwise = (stack, env, code2, ([],[],code):dump, store)

execute (stack, env, JOIN:code, (_,_,code'):dump, store)
    = (stack, env, code', dump, store)

execute (stack, env, HALT:code, dump, store)
    = (stack, env, [], dump, store)

execute conf
    = error ("execute: undefined for " ++ show conf)


-- execution trace starting from an initial state
executeT :: Conf -> [Conf]
executeT conf = trace
    where confs = iterate execute conf
          trace = takeWhile (not.final) confs
          final (s, e, c, d, st) = null c 


-- run a sequence of machine instructions
-- returns the result value 
run :: Code -> Value
run code = value
    where trace = executeT ([],[],code,[],Map.empty)
          (value:_, _, _, _, _) = last trace

runT :: Code -> [Conf]
runT code = executeT ([],[],code,[],Map.empty)


-- compile a lambda term into SECD code
compile :: Term -> [Ident] -> [Instr]

compile (Var x) sym 
    = case elemIndex x sym of
        Nothing -> error ("free variable: " ++ show x)
        Just k -> [LD k]
-- "elemIndex x xs" 
-- gives the index of first occurence of x in xs or Nothing 

compile (Lambda x e) sym 
  = let code = compile e (x:sym) ++ [RTN]
    in [LDF code]

-- compile a recursive function
compile (Fix (Lambda f (Lambda x e1))) sym
  = let code = compile e1 (x:f:sym) ++ [RTN]
    in [LDRF code]

compile (App e1 e2) sym 
  = let code1 = compile e1 sym 
        code2 = compile e2 sym 
    in code1 ++ code2 ++ [AP]
       
compile (Const n) sym = [LDC n]

compile (e1 :+ e2) sym 
  = let code1= compile e1 sym 
        code2= compile e2 sym 
    in code1++code2 ++ [ADD]

compile (e1 :- e2) sym 
  = let code1=compile e1 sym
        code2=compile e2 sym 
    in code1 ++ code2 ++ [SUB]

compile (e1 :* e2) sym 
  = let code1 = compile e1 sym 
        code2 = compile e2 sym 
    in code1 ++ code2 ++ [MUL]

compile (IfZero e1 e2 e3) sym
  = let code1 = compile e1 sym 
        code2 = compile e2 sym ++ [JOIN]
        code3 = compile e3 sym ++ [JOIN]
    in code1 ++ [SEL code2 code3]


compile (Let x e1 e2) sym
    = compile (App (Lambda x e2) e1) sym
 

-- compile the main expression
compileMain :: Term ->  [Instr]
compileMain e = compile e [] ++ [HALT]



