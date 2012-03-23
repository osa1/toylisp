module Lisp where

import Types
import Parser


car :: [Val] -> Env -> (Val, Env)
car ((VList (x:xs)):rest) env = (x, env)

cdr :: [Val] -> Env -> (Val, Env)
cdr ((VList (x:xs)):rest) env = (VList xs, env)

