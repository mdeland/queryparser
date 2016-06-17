module PrintQuery (formatQuery) where

import Control.Monad (when, unless)
import qualified Control.Monad as CM
import qualified Control.Monad.State as State
import qualified Data.Text as T
import qualified Data.List as L
import Data.Maybe
import Debug.Trace

import Types
import CApi


formatQuery :: Node -> String
formatQuery nd = fst $ snd $ State.runState (formatNode nd) ("", 0)

 -- State Helpers
type Output = (String, Int)

append :: String -> State.State Output ()
append s = State.state $ \(s1, i) -> ((), (s1 ++ s, i))

newline :: State.State Output ()
newline = State.state $ \(s, i) ->
    let lastC = last s
    in if lastC == '\t' || lastC == '\n'
        then ((), (s, i))
        else ((), (s ++ "\n" ++ rep i '\t', i))
  where
    rep :: Int -> Char -> String
    rep i c = fmap (const c) [1 .. i]

indent :: State.State Output Int
indent = State.state $ \(s, i) -> (i + 1, (s, i + 1))

unindent :: State.State Output Int
unindent = State.state $ \(s, i) -> (i - 1, (s, i - 1))


-- Format Helpers
formatListNodes :: [Node] -> Bool -> (String, Bool) -> State.State Output ()
formatListNodes (x:y:xs) shouldNewLine (intersperse, intersperseBefore) = do
    formatNode x
    when intersperseBefore $ append intersperse
    when shouldNewLine newline
    unless intersperseBefore $ append intersperse
    formatListNodes (y:xs) shouldNewLine (intersperse, intersperseBefore)
formatListNodes [x] _ _ = formatNode x
formatListNodes [] _ _ = return ()

formatIndentedList :: [Node] -> String -> State.State Output ()
formatIndentedList nds intersperse = do
    let needsIndent = length nds > 1
    if needsIndent
        then do
            indent
            newline
            return ()
        else append " "
    formatListNodes nds True (intersperse, True)
    when needsIndent (CM.void unindent)


formatNode :: Node -> State.State Output ()
formatNode (NodeList nds) = do
    mapM_ formatNode nds
    return ()
formatNode UnhandledNode = append "UNHANDLED"
formatNode (SelectTarget mAlias val) = do
    formatNode val
    when (isJust mAlias) $ do
          append " AS "
          append $ fromJust mAlias
          return ()
    return ()
formatNode (ColumnRef names) = formatListNodes names False (".", True)

formatNode (ConstInt v) = append $ show v
formatNode (ConstFloat v) = append $ show v
formatNode (ConstString v) = append $ "'" ++ v ++ "'"
formatNode (ConstNull) = append "NULL"

formatNode (BoolExpr booltype clauses) =
    case booltype of
      AND_EXPR -> formatListNodes clauses True ("AND ", False)
      OR_EXPR -> formatListNodes clauses True ("OR ", False)
      NOT_EXPR -> do
        append "NOT "
        formatListNodes clauses False ("", False)


formatNode (StringNode s) = append s

formatNode (FuncCall names args orders filter _ _ _ _) = do
    mapM_ formatNode names
    append "("
    mapM_ formatNode args
    append ")"

formatNode (A_Expr exprType _ left right) = do
    formatNode left
    append " = "
    formatNode right

formatNode (JoinExpr joinType left right mQuals _) = do
    formatNode left
    newline
    append "JOIN "
    formatNode right
    mapM_ writeQuals mQuals
    return ()
  where
    writeQuals quals = do
        indent
        newline
        append "ON "
        formatNode quals
        unindent
        newline
formatNode (TableRef db schema table alias) = do
    append $ maybe "" (\d -> d ++ ".") db
    append $ maybe "" (\s -> s ++ ".") schema
    append $ fromMaybe "" table
    append $ maybe "" (\a -> " AS " ++ a) alias

formatNode A_Star = append "*"

-- TODO handle lateral
formatNode (RangeSubselect _ subq mAlias) = do
    append "("
    indent
    newline
    formatNode subq
    unindent
    newline
    append ")"
    when (isJust mAlias) $ do
          append " AS "
          append $ fromJust mAlias
          return ()


formatNode (SelectStmnt targets from mWhere group) = do
    append "SELECT"
    -- select columns
    formatIndentedList targets ","

    unless (null from) $ do
                                newline
                                append "FROM"
                                formatIndentedList from ","
                                return ()
    unless (null group) $ do
                                newline
                                append "GROUP BY"
                                formatIndentedList group ","
                                return ()
    when (isJust mWhere) $ do
                                newline
                                append "WHERE"
                                indent
                                newline
                                mapM_ formatNode mWhere
                                unindent
                                return ()

formatNode nd = append $ "not implemented yet"

