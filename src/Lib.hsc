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
    let s1 = "select a.aa, a.ab as xyz from a join b on a.x = b.y"
    let s2 = "select a.aa from a as c join b on c.x = b.y where c.xx = 'abc'"
    let s3 = "select x, sum(y) from z group by 1"
    P.parseIt s1
    P.parseIt s2
    P.parseIt s3
