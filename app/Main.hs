module Main where

import RV32I.CPU (runCPU')
import RV32I.Programs.Example (addImm)
import Prelude (print, ($), IO)

main :: IO ()
main = print $ runCPU' addImm 0 10
