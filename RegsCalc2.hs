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


-- Solve the following with 1)assuming that registers have
-- some default value and 2) registers don't have default
-- values, i.e., they're not
-- set to a value unless the user does so, e.g.:
-- Set A (Lit 1)
-- so you should manage the case that a register doesn't
-- have a value and a program is trying to get its value.
-- Also your semantic domain should represent this.
--

-- 2. Assuming that registers have some default value
-- Indetify/define the semantic domain for this language.
--    



-- 3. Assuming that registers don't have default
--    values, i.e., they're not
--    set to a value unless the user does so, e.g.:
--    Set A (Lit 1)
--    so you should manage the case that a register doesn't
--    have a value and a program is trying to get its value.
--    Also your semantic domain should represent this.
--






