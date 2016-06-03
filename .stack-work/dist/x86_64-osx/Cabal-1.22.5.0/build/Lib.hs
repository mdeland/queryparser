{-# LINE 1 "src/Lib.hsc" #-}
{-# LANGUAGE CPP, ForeignFunctionInterface, CApiFFI #-}
{-# LINE 2 "src/Lib.hsc" #-}

module Lib
    ( someFunc
    ) where


{-# LINE 8 "src/Lib.hsc" #-}

{-# LINE 9 "src/Lib.hsc" #-}

{-# LINE 10 "src/Lib.hsc" #-}

{-# LINE 11 "src/Lib.hsc" #-}

{-# LINE 12 "src/Lib.hsc" #-}

{-# LINE 13 "src/Lib.hsc" #-}

{-# LINE 14 "src/Lib.hsc" #-}

{-# LINE 15 "src/Lib.hsc" #-}

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
