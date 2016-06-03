{-# LANGUAGE CPP, ForeignFunctionInterface, CApiFFI #-}

module Lib
    ( someFunc
    ) where

#include "stdbool.h"
#include "postgres.h"
#include "postgres_ext.h"
#include "parser/parser.h"
#include "nodes/nodes.h"
#include "nodes/parsenodes.h"
#include "nodes/primnodes.h"
#include "utils/memutils.h"

import Foreign
import Foreign.C
import Foreign.C.Types

foreign import ccall "math.h sin"
     c_sin :: CDouble -> CDouble

-- foreign import ccall "/Users/mdeland/src/postgres/src/include/utils/memutils.h MemoryContextInit"
foreign import ccall "postgres/src/utils/memutils.h MemoryContextInit"
    c_MemoryContextInit :: IO ()

msin :: Double -> Double
msin d = realToFrac (c_sin (realToFrac d))

someFunc :: IO ()
someFunc = do
    putStrLn "calling postgres memory init"
    c_MemoryContextInit
    putStrLn "success"
    print $ msin 1.0
