-- | A simple calculator language with two memory registers.
module RegCalc where

-- 1. Define the abstract syntax.

data Reg = A | B         -- registers
  deriving (Eq,Show)


data Exp = Lit Int       -- a literal integer
         | Neg Exp       -- integer negation
         | Add Exp Exp   -- integer addition
         | Set Reg Exp   -- save result to a register and return it
         | Get Reg       -- return the value of a register
  deriving (Eq,Show)

data Test = LTE Exp Exp
  deriving(Eq,Show)

data Stmt = Set Exp
          | While Test Stmt
          | Begin [Stmt]
  deriving(Eq,Show)




-- 2. Indetify/define the semantic domain for this language.
--    a) Assume that registers are set to zero by default.
--
type Reg = Int
exp :: Exp -> Register -> Int
exp Get     = \r -> reg
exp 

--    b) Registers have no default value, i.e., they're not
--       set to a value unless the user does so, e.g.:
--       Set A (Lit 1)


-- 3. Define the semantic function.
--    a) Assume that registers are set to zero by default.


--    b) There is no default value for registers.

