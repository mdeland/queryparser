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


formatQuery :: Node -> [CommentData] -> String
formatQuery nd cd =  (\(a, _, _, _) -> a) $ snd $ State.runState (formatNode nd) ("", 0, False, cd)

 -- State Helpers
 -- (query string, tab indentation, nested expression)
type Output = (String, Int, Bool, [CommentData])

append :: String -> State.State Output ()
append s = State.state $ \(s1, i, n, cd) ->
     case cd of
         (CommentData (c, commentLoc) : others) | ((length (s1 ++ s)) >= commentLoc) ->
            ((), (s1 ++ c ++ s, i, n, others))
         _ -> ((), (s1 ++ s, i, n, cd))

newline :: State.State Output ()
newline = State.state $ \(s, i, n, cd) ->
    let lastC = last s
    in if lastC == '\t' || lastC == '\n'
        then ((), (s, i, n, cd))
        else ((), (s ++ "\n" ++ rep i '\t', i, n, cd))
  where
    rep :: Int -> Char -> String
    rep i c = fmap (const c) [1 .. i]

indent :: State.State Output Int
indent = State.state $ \(s, i, n, cd) -> (i + 1, (s, i + 1, n, cd))

unindent :: State.State Output Int
unindent = State.state $ \(s, i, n, cd) -> (i - 1, (s, i - 1, n, cd))

setNestExpression :: Bool -> State.State Output ()
setNestExpression n = State.state $ \(s, i, _, cd) -> ((), (s, i, n, cd))

getNestExpression :: State.State Output Bool
getNestExpression = State.state $ \(s, i, n, cd) -> (n, (s, i, n, cd))

-- maybeAddComment :: Location -> State.State Output ()
-- maybeAddComment (Location loc) = State.state $ \(s, i, n, cd) ->
--     case cd of
--         (CommentData (c, commentLoc) : others) | (loc > commentLoc) -> ((), (s ++ c, i, n, others))
--         _ -> ((), (s, i, n, cd))


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

formatListOntoSeparateLines :: [Node] -> String -> State.State Output ()
formatListOntoSeparateLines (x:y:xs) intersperse = do
    formatNode x
    newline
    append intersperse
    newline
    formatListOntoSeparateLines (y:xs) intersperse
formatListOntoSeparateLines [x] _ = formatNode x
formatListOntoSeparateLines [] _ = return ()

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
formatNode (ResTarget mAlias val _) = do
    formatNode val
    when (isJust mAlias) $ do
          append " AS "
          append $ fromJust mAlias
          return ()
    return ()
formatNode (ColumnRef names) = formatListNodes names False (".", True)

formatNode (ConstInt v loc) = do
    append $ show v
    -- maybeAddComment loc
formatNode (ConstFloat v loc) = do
    append $ show v
    -- maybeAddComment loc
formatNode (ConstString v loc) = do
    append $ "'" ++ v ++ "'"
    -- maybeAddComment loc
formatNode (ConstNull loc) = do
    append "NULL"
    -- maybeAddComment loc

formatNode (BoolExpr booltype clauses) = do
    insideExpr <- getNestExpression
    if not insideExpr
      then setNestExpression True
      else do
              append "("
              indent
              newline
    case booltype of
      AND_EXPR -> formatListOntoSeparateLines clauses "AND"
      OR_EXPR -> formatListOntoSeparateLines clauses "OR"
      NOT_EXPR -> do
        append "NOT "
        formatListNodes clauses False ("", False)
    if not insideExpr
      then setNestExpression False
      else do
              unindent
              newline
              append ")"


formatNode (StringNode s) = append s

formatNode (FuncCall names args orders filter _ _ _ _ _) = do
    mapM_ formatNode names
    append "("
    mapM_ formatNode args
    append ")"

formatNode (A_Expr exprType names left right) = do
    formatNode left
    append " "
    formatNode names
    append " "
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


formatNode (SelectStmnt targets from mWhere group mOffset mLimit) = do
    append "SELECT"
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

    when (isJust mLimit) $ do
                                newline
                                append "LIMIT "
                                mapM_ formatNode mLimit
                                return ()

    when (isJust mOffset) $ do
                                newline
                                append "OFFSET "
                                mapM_ formatNode mOffset
                                return ()

formatNode nd = append $ "not implemented yet"

