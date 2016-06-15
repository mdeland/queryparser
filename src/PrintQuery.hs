module PrintQuery where

import Control.Monad (when)
import qualified Control.Monad.State as State
import qualified Data.Text as T
import qualified Data.List as L
import Data.Maybe

import Types
import CApi

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


printQuery :: Node -> String
printQuery nd = fst $ snd $ State.runState (showsNode' nd) ("", 0)

showsNode' :: Node -> State.State Output ()
showsNode' (NodeList nds) = do
    mapM_ showsNode' nds
    return ()
showsNode' UnhandledNode = append "UNHANDLED"
showsNode' (SelectTarget mAlias val) = do
    showsNode' val
    when (isJust mAlias) $ do
          append " AS "
          append $ fromJust mAlias
          return ()
    return ()
showsNode' (ColumnRef names) = append (L.intercalate "." names)

showsNode' (ConstInt v) = append $ show v
showsNode' (ConstFloat v) = append $ show v
showsNode' (ConstString v) = append $ "'" ++ v ++ "'"
showsNode' (ConstNull) = append "NULL"

showsNode' (StringNode s) = append s

showsNode' (FuncCall names args orders filter _ _ _ _) = do
    mapM_ showsNode' names
    append "("
    mapM_ showsNode' args
    append ")"

showsNode' (A_Expr exprType _ left right) = do
    showsNode' left
    append " = "
    showsNode' right

showsNode' (JoinExpr joinType left right mQuals _) = do
    showsNode' left
    newline
    append "JOIN "
    showsNode' right
    mapM_ writeQuals mQuals
    return ()
  where
    writeQuals quals = do
        indent
        newline
        append "ON "
        showsNode' quals
        unindent
        newline
showsNode' (TableRef db schema table alias) = do
    append $ maybe "" (\d -> d ++ ".") db
    append $ maybe "" (\s -> s ++ ".") schema
    append $ fromMaybe "" table
    append $ maybe "" (\a -> " AS " ++ a) alias
showsNode' (SelectStmnt targets from mWhere group) = do
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
              mapM_ showsNode' group
              return ()
        else return ()
    when (isJust mWhere) $ do
                              append "WHERE"
                              indent
                              newline
                              mapM_ showsNode' mWhere
                              unindent
                              newline
  where
    writeTargets ni (x:y:zs) = do
        showsNode' x
        append ","
        newline
        writeTargets ni (y:zs)
    writeTargets ni [x] = do
        showsNode' x
        when ni $ unindent >>= \_ -> return ()
    writeFroms (x:y:zs) = do
        showsNode' x
        newline
        writeFroms (y:zs)
    writeFroms [x] = do
        showsNode' x

showsNode' nd = append $ "not implemented yet"

