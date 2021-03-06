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

nodeInt :: IO CInt -> IO Int
nodeInt = fmap fromIntegral


parse :: Ptr CNode -> IO Node
parse nd = do
    let tag = c_nodeTag nd :: CInt
    let tp = toEnum (fromIntegral tag)
    debug (show $ tp)
    -- parse' nd $ toEnum ( fromIntegral tag )
    parse' nd tp

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
    offsetNode <- (#{peek SelectStmt, limitOffset} nd) >>= nodeMaybe
    limitNode <- (#{peek SelectStmt, limitCount} nd) >>= nodeMaybe

    return $ SelectStmnt targetNode fromNode whereNode groupNode offsetNode limitNode

parse' nd ResTargetTag = do
    debug "resTarget"
    name <- (#{peek ResTarget, name} nd) >>= nodeMaybeString
    val <- (#{peek ResTarget, val} nd) >>= parse
    loc <- nodeInt $ (#{peek ResTarget, location} nd)

    -- TODO will this always return a SelectTarget?

    return $ ResTarget name val (Location loc)

parse' nd ColumnRefTag = do
    fieldsNode <- (#{peek ColumnRef, fields} nd) >>= nodeList
    return $ ColumnRef fieldsNode

parse' nd StringTag = do
    str <- peekCString $ c_strVal nd
    return $ StringNode str

parse' nd A_ConstTag = do
    let tag = c_constType nd
    let constType = toEnum ( fromIntegral tag )
    debug "const"
    debug (show constType)

    iloc <- nodeInt $ (#{peek A_Const, location} nd)
    debug (show iloc)
    let loc = Location iloc
    val <- case constType of
              IntegerTag -> return $ ConstInt (fromIntegral $ c_constInt nd) loc
              FloatTag -> do
                  fl <- (read <$> peekCString (c_constStr nd))
                  return $ ConstFloat fl loc
              StringTag -> do
                  s <- peekCString (c_constStr nd)
                  return $ ConstString s loc
              NullTag -> return $ ConstNull loc
              _ -> do
                  debug $ (show tag) ++ " in const not handled"
                  undefined
    return val

parse' nd BoolExprTag = do
  cboolType <- (#{peek BoolExpr, boolop} nd) :: (IO CInt)
  let boolType = toEnum $ fromIntegral cboolType
  debug $ "bool type: " ++ (show boolType)
  cLoc <- (#{peek BoolExpr, location} nd) :: (IO CInt)
  let loc = fromIntegral cLoc
  debug $ "loc: " ++ (show loc)
  expr <- (#{peek BoolExpr, args} nd) >>= nodeList
  return $ BoolExpr boolType expr

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
    loc <- nodeInt $ (#{peek ResTarget, location} nd)
    -- TODO WindowDef
    return $ FuncCall funcName args aggOrder aggFilter
                      withinGroup aggStar aggDistinct variadic (Location loc)

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

  -- cboolType <- (#{peek BoolExpr, boolop} nd) :: (IO CInt)
  -- let boolType = toEnum $ fromIntegral cboolType
    ckind <- (#{peek A_Expr, kind} nd) :: (IO CInt)
    let kind = toEnum $ fromIntegral ckind
    nameNode <- (#{peek A_Expr, name} nd)
    leftNode <- (#{peek A_Expr, lexpr} nd)
    rightNode <- (#{peek A_Expr, rexpr} nd)
    debug $ "expr type:" ++ (show kind)
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

runParse :: String -> IO Node
runParse s1 = do
    c_MemoryContextInit
    useAsCString (pack s1) $ \s -> do
        let nd = c_raw_parser s
        parse nd
        -- let parsed = formatQuery p cd
        -- return parsed

parseIt :: String -> IO ()
parseIt s1 = do
    print s1
    p <- runParse s1
    let parsed = formatQuery p []
    putStrLn $ parsed
    print parsed
    print "done"
    print "----\n----\n----"
