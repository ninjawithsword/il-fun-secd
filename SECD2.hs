{-# LANGUAGE DeriveFunctor, FlexibleInstances #-}
{- -----------------------------------
   Fun: a minimal functional language
   -----------------------------------
   A byte-code compiler for a SECD-like virtual machine.
   
   Pedro Vasconcelos, 2008--2013.
 -}
module SECD2 where
import Fun
import Data.List (elemIndex)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State

-----------------------------------------------------------------
-- SECD machine definitions
-----------------------------------------------------------------

-- pseudo instructions parameterized by label type
data Instr l = HALT            -- finished
             | LDC Int         -- load constant
             | LD Int          -- load variable
             | ADD             -- addition
             | SUB             -- subtraction
             | MUL             -- multiplication
             | SEL l l         -- select zero/non-zero
             | LDF l           -- load a closure
             | LDRF l          -- load a recursive closure
             | AP              -- apply
             | RTN             -- return 
             | JOIN            -- close branch
             | PAIR            -- see compile below
             | FST
             | SND
               deriving (Show, Functor)

-- symbolic labels are just strings
type Label = String

-- State monad for code generation 
-- generating fresh labels and emitting global code
type CodeGen = State (Map Label [Instr Label])

-- add a new global code segment
-- returns new label
newGlobal :: [Instr Label] -> CodeGen Label
newGlobal c = do labels <- get
                 let l = "L" ++ show (1+Map.size labels) 
                 put (Map.insert l c labels)
                 return l


-- compile a lambda term into SECD code
compile :: Term -> [Ident] -> CodeGen [Instr Label]

compile (Var x) sym 
    = case elemIndex x sym of
        Nothing -> error ("free variable: " ++ show x)
        Just k -> return [LD k]
-- "elemIndex x xs" 
-- gives the index of first occurence of x in xs or Nothing 

compile (Lambda x e) sym 
  = do code <- compile e (x:sym) 
       l <- newGlobal (code++[RTN])
       return [LDF l]

-- compile a recursive function
compile (Fix (Lambda f (Lambda x e1))) sym
  = do code <- compile e1 (x:f:sym) 
       l <- newGlobal (code++[RTN])
       return [LDRF l]

compile (App e1 e2) sym 
  = do code1 <- compile e1 sym 
       code2 <- compile e2 sym 
       return (code1 ++ code2 ++ [AP])
       
compile (Const n) sym = return [LDC n]

compile (e1 :+ e2) sym 
  = do code1<-compile e1 sym 
       code2<-compile e2 sym 
       return (code1++code2 ++ [ADD])

compile (e1 :- e2) sym 
  = do code1<-compile e1 sym
       code2<-compile e2 sym 
       return (code1 ++ code2 ++ [SUB])

compile (e1 :* e2) sym 
  = do code1<-compile e1 sym 
       code2<-compile e2 sym 
       return (code1 ++ code2 ++ [MUL])

compile (IfZero e1 e2 e3) sym
  = do code1 <- compile e1 sym  
       code2 <- compile e2 sym
       code3 <- compile e3 sym
       ltrue <- newGlobal (code2 ++ [JOIN])
       lfalse<- newGlobal (code3 ++ [JOIN])
       return (code1 ++ [SEL ltrue lfalse])

compile (Let x e1 e2) sym
    = compile (App (Lambda x e2) e1) sym

compile (Pair e1 e2) sym
    = do code1<-compile e1 sym
         code2<-compile e2 sym
         return (code1 ++ code2 ++ [PAIR])  

compile (Fst e) sym
    = do code<-compile e sym
         return (code ++ [FST])

compile (Snd e) sym
    = do code<-compile e sym
         return (code ++ [SND])

-- compile the main expression
compileMain :: Term -> CodeGen [Instr Label]
compileMain e = do code<-compile e [] 
                   return (code ++ [HALT])

runCodeGen :: CodeGen [Instr Label] -> Map Label [Instr Label]
runCodeGen cgen =  Map.insert "L0" c0 labels  -- code start
  where (c0, labels) = runState cgen Map.empty


-- label resolution
-- code addresses are simple integers
type Addr = Int

resolveLabels :: Map Label [Instr Label] -> [Instr Addr]
resolveLabels labels = map resolve $ concat (Map.elems labels)
  where table = symbolTable labels
        resolve = fmap (\l -> Map.findWithDefault undefined l table)

symbolTable :: Map Label [Instr Label] -> Map Label Addr
symbolTable labels = Map.fromList (zip (Map.keys labels) addrs)
  where sizes = map (\c -> sum [sizeof i | i<-c]) (Map.elems labels)
        addrs = scanl (+) 0 sizes


type Bytecode = Int -- bytecodes are plain integers

-- assemblying into bytecodes
class Asm a where
  assemble :: a -> [Bytecode]
  
instance Asm (Instr Addr) where
  assemble HALT      = [0]
  assemble (LDC n)   = [1, n]
  assemble (LD n)    = [2, n]
  assemble ADD       = [3]
  assemble SUB       = [4]
  assemble MUL       = [5]
  assemble (SEL l1 l2) = [6, l1, l2]
  assemble (LDF l)   = [7, l]
  assemble (LDRF l)  = [8, l]
  assemble AP        = [9]
  assemble RTN       = [10]
  assemble JOIN      = [11]
  assemble PAIR      = [12]
  assemble FST       = [13]
  assemble SND       = [14]

instance Asm a => Asm [a] where
  assemble = concatMap assemble

sizeof :: Instr l -> Int
sizeof = length . assemble . fmap (\_ -> 0 :: Int)

writeBytecode :: FilePath -> [Bytecode] -> IO ()
writeBytecode file code = writeFile file (unlines $ map show code)
