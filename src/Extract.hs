module Extract (extractTables) where

import Control.Monad (when, unless)
import qualified Control.Monad as CM
import qualified Control.Monad.State as State
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.List as L
import Data.Maybe
import Debug.Trace

import Types
import CApi

-- type TableSet = HS.HashSet String

extractTables :: Node -> HS.HashSet String
extractTables nd = extractTables' HS.empty nd

extractTables' :: HS.HashSet String -> Node -> HS.HashSet String
extractTables' tables (NodeList nds) =
    HS.unions $ fmap (extractTables' tables) nds
extractTables' tables (SelectStmnt targets from mWhere group _ _) =
    HS.unions $ fmap (extractTables' tables) targets
              ++ fmap (extractTables' tables) from
              ++ maybe [] (replicate 1 . extractTables' tables) mWhere
extractTables' tables (TableRef db schema table _) =
    HS.insert
        ( maybe "" (++ ".") db
          ++ maybe "" (++ ".") schema
          ++ fromMaybe "" table
        ) tables
extractTables' tables (JoinExpr _ left right _ _) =
    HS.union (extractTables' tables left) (extractTables' tables right)
extractTables' tables (RangeSubselect _ sub _) =
    extractTables' tables sub
extractTables' tables _ = tables
