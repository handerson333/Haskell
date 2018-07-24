module KarelSemantics where

import Prelude hiding (Either(..))
import Data.Function (fix)

import KarelSyntax
import KarelState


-- | Valuation function for Test.
test :: Test -> World -> Robot -> Bool
test (Not t)      w r = not (test t w r)                                -- Not test
test (Facing c)   w r = c == (getFacing r)                              -- Facing Cardinal Direction
test (Clear d)    w r = isClear (neighbor (getFacing r) (getPos r)) w   -- Clear Dir                
test (Beeper)     w r = hasBeeper (getPos r) w                          -- Current location has a beeper?
test (Empty)      w r = isEmpty r



-- | Valuation function for Stmt.
stmt :: Stmt -> Defs -> World -> Robot -> Result
stmt Shutdown   _ _ r = Done r
stmt PickBeeper _ w r = let p = getPos r
                        in if hasBeeper p w
                              then OK (decBeeper p w) (incBag r)
                              else Error ("No beeper to pick at: " ++ show p)
stmt Move               _ w r =     if (test (Clear Front) w r) 
                                    then OK w (updatePos (neighbor (getFacing r)) r) 
                                    else Error "That direction is blocked"
stmt PutBeeper          _ w r =     if not (isEmpty r) 
                                    then OK (incBeeper (relativePos Front r) w) (decBag r)   
                                    else Error "You dont have any beepers to place"                        --if bag is not empty, put beeper ahead and decrement bags beeper, else error
stmt (Turn d)           _ w r = OK w (updateFacing (cardTurn d) r)
stmt (Call m)           d w r = case lookup m d of
                                    Just s      -> stmt s d w r
                                    _           -> Error ("Couldn't do" ++ m)
stmt (Iterate i s)      d w r = if i > 1 then case stmt s d w r of
                                                Done  r'    -> Done r'
                                                OK    w' r' -> stmt (Iterate (i-1) s) d w' r'                       
                                                Error e     -> Error e
                                    else stmt s d w r       
stmt (If t s1 s2)       d w r = undefined
stmt (While t s)        d w r = undefined
stmt (Block (x:xs))     d w r = undefined

    
-- | Run a Karel program.
prog :: Prog -> World -> Robot -> Result
prog (m,s) w r = stmt s m w r
