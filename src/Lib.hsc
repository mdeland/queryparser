{-# LANGUAGE CPP, ForeignFunctionInterface, CApiFFI #-}

module Lib
    ( someFunc
    ) where

import Data.ByteString (useAsCString)
import Data.ByteString.Char8 (pack)
import Foreign
import Foreign.C
import Foreign.C.Types

import qualified Parse as P

foreign import ccall "math.h sin"
     c_sin :: CDouble -> CDouble

foreign import ccall "./postgres/src/utils/memutils.h MemoryContextInit"
    c_MemoryContextInit :: IO ()

data CNode = CNode ()

foreign import ccall "./postgres/src/include/parser/parser.h raw_parser"
    c_raw_parser:: CString -> Ptr CNode

msin :: Double -> Double
msin d = realToFrac (c_sin (realToFrac d))

someFunc :: IO ()
someFunc = do
    putStrLn "testing c call"
    print $ msin 1.0
    putStrLn "success"
    putStrLn "calling postgres memory init"
    c_MemoryContextInit
    putStrLn "success"
    P.parseIt

