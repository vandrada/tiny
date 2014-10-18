--------------------------------------------------------------------------------
-- Environment.hs
--
-- Functions related to the environment of a Tiny program
--------------------------------------------------------------------------------
module Environment (
    Var,
    Val,
    Environment,
    emptyEnvironment,
    updateVar
) where

import Data.Map as M

type Var = Char
type Val = Double
type Environment = M.Map Var Val

-- New environment
emptyEnvironment :: M.Map Var Val
emptyEnvironment = M.empty

-- Updates or adds a new variable
updateVar :: Var -> Val -> Environment -> Environment
updateVar = M.insert
