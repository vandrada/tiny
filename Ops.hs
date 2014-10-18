module Tiny (
    (!+),   -- addition
    (!-),   -- subtraction
    (!*),   -- multiplication
    (!/),   -- division
    (!=),   -- assignment
    (!<))   -- printing
where

import Control.Monad.State
import Environment

--
-- Base functions
--
incWith :: Val -> State Val ()
incWith x = state $ \s -> ((), s + x)

subWith :: Val -> State Val ()
subWith x = state $ \s -> ((), x - s)

multWith :: Val -> State Val ()
multWith x = state $ \s -> ((), s * x)

divWith :: Val -> State Val ()
divWith x = state $ \s -> ((), x / s)

toScreen :: Val -> IO ()
toScreen = print

fromScreen :: Var -> Val -> State Environment ()
fromScreen var val = do
    currentEnvironment <- get
    put $ updateVar var val currentEnvironment

fromLine :: Var -> IO (Val, Var)
fromLine var = do
    value <- liftIO getLine
    return (read value, var)

addVar :: Var -> Val -> State Environment ()
addVar var val = do
    currentEnvironment <- get
    put $ updateVar var val currentEnvironment

--
-- Ops
--
(!-) :: Val -> Val -> Val
(!-) rhs = execState (subWith rhs)

(!+) :: Val -> Val -> Val
(!+) rhs = execState (incWith rhs)

(!*) :: Val -> Val -> Val
(!*) rhs = execState (multWith rhs)

(!/) :: Val -> Val -> Val
(!/) rhs = execState (divWith rhs)

(!<) :: t -> Val -> IO ()
(!<) _ = toScreen

(!=) :: Var -> Val -> Environment
(!=) var val = execState (addVar var val) emptyEnvironment
