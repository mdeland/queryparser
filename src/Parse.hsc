{-# LANGUAGE CPP, ForeignFunctionInterface, CApiFFI #-}

module Parse
    ( parseIt,
      runParse
    ) where

import Data.ByteString (useAsCString)
import Data.ByteString.Char8 (pack)
import Data.Void
import Foreign
import Foreign.C
import Foreign.C.Types

import Types
import CApi
import PrintQuery

import Debug.Trace

#include "queryparser.h"
#include "c.h"
#include "nodes/pg_list.h"
#include "nodes/nodes.h"
#include "nodes/parsenodes.h"
#include "nodes/value.h"

debug :: String -> IO ()
debug s = traceIO $ "***** " ++ s
-- debug _ = return ()

nodeList :: Ptr CNode -> IO [Node]
nodeList nl = do
    if (nl == nullPtr)
        then return []
        else extractList <$> parse nl

nodeMaybe :: Ptr CNode -> IO (Maybe Node)
nodeMaybe nm = if (nm == nullPtr)
                  then return Nothing
                  else Just <$> parse nm

nodeMaybeString :: CString -> IO (Maybe String)
nodeMaybeString nd = if (nd /= nullPtr)
                        then Just <$> peekCString nd
                        else return Nothing


parse :: Ptr CNode -> IO Node
parse nd = do
    let tag = c_nodeTag nd
    debug (show tag)
    parse' nd $ toEnum ( fromIntegral tag )

parse' :: Ptr CNode -> NodeTag -> IO Node
parse' nd ListTag = do
    headCell <- (#{peek List, head} nd)
    traverse headCell (NodeList [])
  where
    traverse :: (Ptr CNode) -> Node -> IO Node
    traverse cell (NodeList ls) = do
        let cellNode = c_celldata cell
        nd <- parse cellNode
        newList <- return $ NodeList (ls ++ [nd])
        nextCell <- (#{peek ListCell, next} cell)
        if (nextCell /= nullPtr)
          then traverse nextCell newList
          else return newList

parse' nd SelectStmntTag = do
    targetNode <- (#{peek SelectStmt, targetList} nd) >>= nodeList
    fromNode <- (#{peek SelectStmt, fromClause} nd) >>= nodeList
    whereNode <- (#{peek SelectStmt, whereClause} nd) >>= nodeMaybe
    groupNode <- (#{peek SelectStmt, groupClause} nd) >>= nodeList

    return $ SelectStmnt targetNode fromNode whereNode groupNode

parse' nd ResTargetTag = do
    debug "resTarget"
    name <- (#{peek ResTarget, name} nd) >>= nodeMaybeString
    val <- (#{peek ResTarget, val} nd) >>= parse
    -- TODO will this always return a SelectTarget?
    return $ SelectTarget name val

parse' nd ColumnRefTag = do
    fieldsNode <- (#{peek ColumnRef, fields} nd) >>= nodeList
    return $ ColumnRef fieldsNode

parse' nd StringTag = do
    str <- peekCString $ c_strVal nd
    return $ StringNode str

parse' nd A_ConstTag = do
    let tag = c_constType nd
    debug (show tag)
    let constType = toEnum ( fromIntegral tag )
    val <- case constType of
              IntegerTag -> return $ ConstInt $ fromIntegral $ c_constInt nd
              FloatTag -> fmap ConstFloat (read <$> peekCString (c_constStr nd))
              StringTag -> ConstString <$> peekCString (c_constStr nd)
              NullTag -> return ConstNull
              _ -> do
                  debug $ (show tag) ++ " in const not handled"
                  undefined
    return val


parse' nd JoinExprTag = do
    cjoinType <- (#{peek JoinExpr, jointype} nd) :: (IO CInt)
    let joinType = fromIntegral cjoinType
    debug "join type:"
    debug (show joinType)
    left <- (#{peek JoinExpr, larg} nd) >>= parse
    right <- (#{peek JoinExpr, rarg} nd) >>= parse
    -- TODO optional?
    -- TODO extractList from qual here??
    qual <- (#{peek JoinExpr, quals} nd) >>= parse
    -- TODO Alias

    return $ JoinExpr joinType left right (Just qual) Nothing

parse' nd FuncCallTag = do
    debug "func"
    funcName <- (#{peek FuncCall, funcname} nd) >>= nodeList
    args <- (#{peek FuncCall, args} nd) >>= nodeList
    aggOrder <- (#{peek FuncCall, agg_order} nd) >>= nodeList
    aggFilter <- (#{peek FuncCall, agg_filter} nd) >>= nodeMaybe
    withinGroup <- (#{peek FuncCall, agg_within_group} nd) -- bool
    aggStar <- (#{peek FuncCall, agg_star} nd)             -- bool
    aggDistinct <- (#{peek FuncCall, agg_distinct} nd)     -- bool
    variadic <- (#{peek FuncCall, func_variadic} nd)       -- bool
    -- TODO WindowDef
    return $ FuncCall funcName args aggOrder aggFilter withinGroup aggStar aggDistinct variadic

parse' nd A_StarTag = return A_Star

parse' nd RangeSubselectTag = do
    lateral <- (#{peek RangeSubselect, lateral} nd)
    subquery <- (#{peek RangeSubselect, subquery} nd) >>= parse
    aliasNode <- (#{peek RangeSubselect, alias} nd)
    alias <- if (aliasNode /= nullPtr)
        then do
                calias <- (#{peek Alias, aliasname} aliasNode) :: IO CString
                Just <$> peekCString calias
        else return Nothing
    return $ RangeSubselect lateral subquery alias

parse' nd RangeVarTag = do
    debug "table"
    db <- (#{peek RangeVar, catalogname} nd) >>= nodeMaybeString
    schema <- (#{peek RangeVar, schemaname} nd) >>= nodeMaybeString
    table <- (#{peek RangeVar, relname} nd) >>= nodeMaybeString
    aliasNode <- (#{peek RangeVar, alias} nd)
    alias <- if (aliasNode /= nullPtr)
      then do
              calias <- (#{peek Alias, aliasname} aliasNode) :: IO CString
              Just <$> peekCString calias
      else return Nothing
    return $ TableRef db schema table alias

parse' nd A_ExprTag = do
    debug "expr"
    kind <- (#{peek A_Expr, kind} nd)
    nameNode <- (#{peek A_Expr, name} nd)
    leftNode <- (#{peek A_Expr, lexpr} nd)
    rightNode <- (#{peek A_Expr, rexpr} nd)
    name <- parse nameNode
    left <- parse leftNode
    right <- parse rightNode
    return $ A_Expr kind name left right

parse' nd AliasTag = do
    calias <- (#{peek Alias, aliasname} nd)
    alias <- peekCString calias
    cols <- (#{peek Alias, colnames} nd) >>= nodeMaybe

    return $ Alias alias cols

parse' _ t = do
    debug "not handled yet"
    debug (show t)
    return UnhandledNode


extractList :: Node -> [Node]
extractList (NodeList nd) = nd
extractList _ = trace "extractList undefined" $ undefined

runParse :: String -> IO String
runParse s1 = do
    c_MemoryContextInit
    useAsCString (pack s1) $ \s -> do
        let nd = c_raw_parser s
        p <- parse nd
        let parsed = printQuery p
        return parsed

parseIt :: String -> IO ()
parseIt s1 = do
    print s1
    parsed <- runParse s1
    putStrLn $ parsed
    print parsed
    print "done"
    print "----\n----\n----"
