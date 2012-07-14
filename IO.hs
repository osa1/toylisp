{-# OPTIONS_GHC -Wall -fno-warn-hi-shadowing -fno-warn-unused-do-bind -fno-warn-name-shadowing #-}

-- IO functions

module IO where

import Types
import Data.List (intercalate)
import Control.Monad.IO.Class (liftIO)

println :: SimpleFunc
println args = liftIO (putStrLn $ intercalate "    " (map show args)) >> return Nil

