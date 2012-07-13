-- IO functions

module IO where

import Types
import Data.List (intercalate)
import Control.Monad.IO.Class (liftIO)

println :: SimpleFunc
println args = liftIO (putStrLn $ intercalate "    " (map show args)) >> return Nil

