module PrintQuery (formatQuery) where

import Control.Monad (when)
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
formatListNodes :: [Node] -> Bool -> String -> State.State Output ()
formatListNodes (x:y:xs) shouldNewLine intersperse = do
    formatNode x
    append intersperse
    when shouldNewLine newline
    formatListNodes (y:xs) shouldNewLine intersperse
formatListNodes [x] _ _ = formatNode x
formatListNodes [] _ _ = return ()


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
formatNode (ColumnRef names) = formatListNodes names False "."

formatNode (ConstInt v) = append $ show v
formatNode (ConstFloat v) = append $ show v
formatNode (ConstString v) = append $ "'" ++ v ++ "'"
formatNode (ConstNull) = append "NULL"

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
    let needsIndent = length targets > 1
    -- select columns
    if needsIndent
        then do
            indent
            newline
            return ()
        else append " "
    writeTargets needsIndent targets

    -- from clause
    if (length from) > 0
        then do
            newline
            append "FROM "
            writeFroms from
            return ()
        else return ()
    if (length group) > 0
        then do
              newline
              append "GROUP BY "
              -- TODO
              mapM_ formatNode group
              return ()
        else return ()
    when (isJust mWhere) $ do
                              newline
                              append "WHERE"
                              indent
                              newline
                              mapM_ formatNode mWhere
                              unindent
                              return ()
  where
    writeTargets ni (x:y:zs) = do
        formatNode x
        append ","
        newline
        writeTargets ni (y:zs)
    writeTargets ni [x] = do
        formatNode x
        when ni $ unindent >>= \_ -> return ()
    writeFroms (x:y:zs) = do
        formatNode x
        newline
        writeFroms (y:zs)
    writeFroms [x] = formatNode x

formatNode nd = append $ "not implemented yet"

