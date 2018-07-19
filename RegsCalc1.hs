-- | A simple calculator language with two memory registers.
module RegCalc where


-- 1. Define the abstract syntax.

data Reg = A | B         -- registers
  deriving (Eq,Show)


data Exp = Lit Int       -- a literal integer
         | Neg Exp       -- integer negation
         | Set Reg Exp   -- save result to a register and return it
         | Get Reg       -- return the value of a register
  deriving (Eq,Show)

-- Assumption: registers are set to zero by default.
-- Note that you can just assume this and you don't need
-- to implement this in your sementic function. The same 
-- way that we assumed the one register in the While.hs
-- file is set to some default value and we didn't worry
-- about it.
-- 2. Indetify/define the semantic domain for this language.
--    

type Regs = (Int, Int) 
-- type Domain = Regs -> (Regs, Int)


-- 3. Define the semantic function.
--    

sem :: Exp -> Regs -> (Regs, Int)
sem (Lit i) = \regs -> (regs, i)
sem (Neg e) = \regs -> let result = sem e regs in (fst result, -(snd result))
sem (Set A e) = \regs -> let x = sem e regs in ((snd x , snd regs), snd x)
sem (Set B e) = \regs -> let x = sem e regs in ((fst regs, snd x), snd x)
sem (Get r)   = \regs -> case r of 
                          A -> (regs , fst regs)
                          B -> (regs , snd regs)
                            

-- sem (Get i) = \regs

test :: Exp
test = Get A