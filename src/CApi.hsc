{-# LANGUAGE CPP, ForeignFunctionInterface, CApiFFI #-}

module CApi where

import Foreign
import Foreign.C
import Foreign.C.Types

#include "queryparser.h"
#include "c.h"
#include "nodes/pg_list.h"
#include "nodes/nodes.h"
#include "nodes/parsenodes.h"
-- #include "nodes/value.h"

-- We won't dereference pointers to Nodes
data CNode = CNode ()

foreign import ccall "./postgres/src/utils/memutils.h MemoryContextInit"
    c_MemoryContextInit :: IO ()

foreign import ccall "./postgres/src/include/parser/parser.h raw_parser"
    c_raw_parser:: CString -> Ptr CNode

-- foreign import capi ".postgres/src/include/nodes/nodes.h nodeTag"
foreign import capi "./queryparser.h my_nodeTag"
    c_nodeTag :: Ptr CNode -> CInt

foreign import capi "./queryparser.h const_type"
    c_constType:: Ptr CNode -> CInt

foreign import capi "./queryparser.h const_to_integer"
    c_constInt:: Ptr CNode -> CLong

foreign import capi "./queryparser.h const_to_str"
    c_constStr :: Ptr CNode -> CString

foreign import capi "./postgres/src/include/nodes/pg_list.h linitial"
    c_linitial:: Ptr CNode -> Ptr CNode

foreign import ccall "./queryparser.h cellData"
    c_celldata:: Ptr CNode -> Ptr CNode

-- foreign import capi "./postgres/src/include/nodes/value.h strVal"
--     c_strVal:: Ptr CNode -> CString
foreign import capi "./queryparser.h my_strVal"
     c_strVal:: Ptr CNode -> CString

foreign import capi "./queryparser.h my_intVal"
     c_intVal:: Ptr CNode -> CLong
