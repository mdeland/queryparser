{-# LANGUAGE CPP, ForeignFunctionInterface, CApiFFI #-}

module Parse
    ( parseIt
    ) where

import Data.ByteString (useAsCString)
import Data.ByteString.Char8 (pack)
import Data.Void
import Foreign
import Foreign.C
import Foreign.C.Types

import Types

#include "queryparser.h"
#include "c.h"
#include "nodes/pg_list.h"
#include "nodes/nodes.h"
#include "nodes/parsenodes.h"
-- #include "nodes/value.h"

foreign import ccall "./postgres/src/utils/memutils.h MemoryContextInit"
    c_MemoryContextInit :: IO ()


data NodeTag = ListTag
             | SelectStmntTag
             -- primitive nodes
             | AliasTag
             | RangeVarTag
             | ExprTag
             | VarTag
             | ConstTag
             | ParamTag
             | AggrefTag
             | GroupingFuncTag
             | WindowFuncTag
             | ArrayRefTag
             | FuncExprTag
             | NamedArgExprTag
             | OpExprTag
             | DistinctExprTag
             | NullIfExprTag
             | ScalarArrayOpExprTag
             | BoolExprTag
             | SubLinkTag
             | SubPlanTag
             | AlternativeSubPlanTag
             | FieldSelectTag
             | FieldStoreTag
             | RelabelTypeTag
             | CoerceViaIOTag
             | ArrayCoerceExprTag
             | ConvertRowtypeExprTag
             | CollateExprTag
             | CaseExprTag
             | CaseWhenTag
             | CaseTestExprTag
             | ArrayExprTag
             | RowExprTag
             | RowCompareExprTag
             | CoalesceExprTag
             | MinMaxExprTag
             | XmlExprTag
             | NullTestTag
             | BooleanTestTag
             | CoerceToDomainTag
             | CoerceToDomainValueTag
             | SetToDefaultTag
             | CurrentOfExprTag
             | InferenceElemTag
             | TargetEntryTag
             | RangeTblRefTag
             | JoinExprTag
             | FromExprTag
             | OnConflictExprTag
             | IntoClauseTag
             -- Values
             | ValueTag
             | IntegerTag
             | FloatTag
             | StringTag
             | BitStringTag
             | NullTag
             -- parse nodes
             | A_ExprTag
             | ColumnRefTag
             | ParamRefTag
             | A_ConstTag
             | FuncCallTag
             | A_StarTag
             | A_IndicesTag
             | A_IndirectionTag
             | A_ArrayExprTag
             | ResTargetTag
             | MultiAssignRefTag
             | TypeCastTag
             | CollateClauseTag
             | SortByTag
             | WindowDefTag
             | RangeSubselectTag
             | RangeFunctionTag
             | RangeTableSampleTag
             | TypeNameTag
             | ColumnDefTag
             | IndexElemTag
             | ConstraintTag
             | DefElemTag
             | RangeTblEntryTag
             | RangeTblFunctionTag
             | TableSampleClauseTag
             | WithCheckOptionTag
             | SortGroupClauseTag
             | GroupingSetTag
             | WindowClauseTag
             | FuncWithArgsTag
             | AccessPrivTag
             | CreateOpClassItemTag
             | TableLikeClauseTag
             | FunctionParameterTag
             | LockingClauseTag
             | RowMarkClauseTag
             | XmlSerializeTag
             | WithClauseTag
             | InferClauseTag
             | OnConflictClauseTag
             | CommonTableExprTag
             | RoleSpecTag
             deriving (Eq,Show)

instance Enum NodeTag where
  fromEnum SelectStmntTag = 705
  fromEnum ValueTag = 650
  fromEnum IntegerTag = 651
  fromEnum FloatTag = 652
  fromEnum StringTag = 653
  fromEnum BitStringTag = 654
  fromEnum NullTag = 655
  fromEnum ListTag = 656
  -- primitive nodes
  fromEnum AliasTag = 300
  fromEnum RangeVarTag = 301
  fromEnum ExprTag = 302
  fromEnum VarTag = 303
  fromEnum ConstTag = 304
  fromEnum ParamTag = 305
  fromEnum AggrefTag = 306
  fromEnum GroupingFuncTag = 307
  fromEnum WindowFuncTag = 308
  fromEnum ArrayRefTag = 309
  fromEnum FuncExprTag = 310
  fromEnum NamedArgExprTag = 311
  fromEnum OpExprTag = 312
  fromEnum DistinctExprTag = 313
  fromEnum NullIfExprTag = 314
  fromEnum ScalarArrayOpExprTag = 315
  fromEnum BoolExprTag = 316
  fromEnum SubLinkTag = 317
  fromEnum SubPlanTag = 318
  fromEnum AlternativeSubPlanTag = 319
  fromEnum FieldSelectTag = 320
  fromEnum FieldStoreTag = 321
  fromEnum RelabelTypeTag = 322
  fromEnum CoerceViaIOTag = 323
  fromEnum ArrayCoerceExprTag = 324
  fromEnum ConvertRowtypeExprTag = 325
  fromEnum CollateExprTag = 326
  fromEnum CaseExprTag = 327
  fromEnum CaseWhenTag = 328
  fromEnum CaseTestExprTag = 329
  fromEnum ArrayExprTag = 330
  fromEnum RowExprTag = 331
  fromEnum RowCompareExprTag = 332
  fromEnum CoalesceExprTag = 333
  fromEnum MinMaxExprTag = 334
  fromEnum XmlExprTag = 335
  fromEnum NullTestTag = 336
  fromEnum BooleanTestTag = 337
  fromEnum CoerceToDomainTag = 338
  fromEnum CoerceToDomainValueTag = 339
  fromEnum SetToDefaultTag = 340
  fromEnum CurrentOfExprTag = 341
  fromEnum InferenceElemTag = 342
  fromEnum TargetEntryTag = 343
  fromEnum RangeTblRefTag = 344
  fromEnum JoinExprTag = 345
  fromEnum FromExprTag = 346
  fromEnum OnConflictExprTag = 347
  fromEnum IntoClauseTag = 348
  fromEnum A_ExprTag = 900
  fromEnum ColumnRefTag = 901
  fromEnum ParamRefTag = 902
  fromEnum A_ConstTag = 903
  fromEnum FuncCallTag = 904
  fromEnum A_StarTag = 905
  fromEnum A_IndicesTag = 906
  fromEnum A_IndirectionTag = 907
  fromEnum A_ArrayExprTag = 908
  fromEnum ResTargetTag = 909
  fromEnum MultiAssignRefTag = 910
  fromEnum TypeCastTag = 911
  fromEnum CollateClauseTag = 912
  fromEnum SortByTag = 913
  fromEnum WindowDefTag = 914
  fromEnum RangeSubselectTag = 915
  fromEnum RangeFunctionTag = 916
  fromEnum RangeTableSampleTag = 917
  fromEnum TypeNameTag = 918
  fromEnum ColumnDefTag = 919
  fromEnum IndexElemTag = 920
  fromEnum ConstraintTag = 921
  fromEnum DefElemTag = 922
  fromEnum RangeTblEntryTag = 923
  fromEnum RangeTblFunctionTag = 924
  fromEnum TableSampleClauseTag = 925
  fromEnum WithCheckOptionTag = 926
  fromEnum SortGroupClauseTag = 927
  fromEnum GroupingSetTag = 928
  fromEnum WindowClauseTag = 929
  fromEnum FuncWithArgsTag = 930
  fromEnum AccessPrivTag = 931
  fromEnum CreateOpClassItemTag = 932
  fromEnum TableLikeClauseTag = 933
  fromEnum FunctionParameterTag = 934
  fromEnum LockingClauseTag = 935
  fromEnum RowMarkClauseTag = 936
  fromEnum XmlSerializeTag = 937
  fromEnum WithClauseTag = 938
  fromEnum InferClauseTag = 939
  fromEnum OnConflictClauseTag = 940
  fromEnum CommonTableExprTag = 941
  fromEnum RoleSpecTag = 942

  toEnum 705 = SelectStmntTag
  toEnum 650 = ValueTag
  toEnum 651 = IntegerTag
  toEnum 652 = FloatTag
  toEnum 653 = StringTag
  toEnum 654 = BitStringTag
  toEnum 655 = NullTag
  toEnum 656 = ListTag
  toEnum 300 = AliasTag
  toEnum 301 = RangeVarTag
  toEnum 302 = ExprTag
  toEnum 303 = VarTag
  toEnum 304 = ConstTag
  toEnum 305 = ParamTag
  toEnum 306 = AggrefTag
  toEnum 307 = GroupingFuncTag
  toEnum 308 = WindowFuncTag
  toEnum 309 = ArrayRefTag
  toEnum 310 = FuncExprTag
  toEnum 311 = NamedArgExprTag
  toEnum 312 = OpExprTag
  toEnum 313 = DistinctExprTag
  toEnum 314 = NullIfExprTag
  toEnum 315 = ScalarArrayOpExprTag
  toEnum 316 = BoolExprTag
  toEnum 317 = SubLinkTag
  toEnum 318 = SubPlanTag
  toEnum 319 = AlternativeSubPlanTag
  toEnum 320 = FieldSelectTag
  toEnum 321 = FieldStoreTag
  toEnum 322 = RelabelTypeTag
  toEnum 323 = CoerceViaIOTag
  toEnum 324 = ArrayCoerceExprTag
  toEnum 325 = ConvertRowtypeExprTag
  toEnum 326 = CollateExprTag
  toEnum 327 = CaseExprTag
  toEnum 328 = CaseWhenTag
  toEnum 329 = CaseTestExprTag
  toEnum 330 = ArrayExprTag
  toEnum 331 = RowExprTag
  toEnum 332 = RowCompareExprTag
  toEnum 333 = CoalesceExprTag
  toEnum 334 = MinMaxExprTag
  toEnum 335 = XmlExprTag
  toEnum 336 = NullTestTag
  toEnum 337 = BooleanTestTag
  toEnum 338 = CoerceToDomainTag
  toEnum 339 = CoerceToDomainValueTag
  toEnum 340 = SetToDefaultTag
  toEnum 341 = CurrentOfExprTag
  toEnum 342 = InferenceElemTag
  toEnum 343 = TargetEntryTag
  toEnum 344 = RangeTblRefTag
  toEnum 345 = JoinExprTag
  toEnum 346 = FromExprTag
  toEnum 347 = OnConflictExprTag
  toEnum 348 = IntoClauseTag
  toEnum 900 = A_ExprTag
  toEnum 901 = ColumnRefTag
  toEnum 902 = ParamRefTag
  toEnum 903 = A_ConstTag
  toEnum 904 = FuncCallTag
  toEnum 905 = A_StarTag
  toEnum 906 = A_IndicesTag
  toEnum 907 = A_IndirectionTag
  toEnum 908 = A_ArrayExprTag
  toEnum 909 = ResTargetTag
  toEnum 910 = MultiAssignRefTag
  toEnum 911 = TypeCastTag
  toEnum 912 = CollateClauseTag
  toEnum 913 = SortByTag
  toEnum 914 = WindowDefTag
  toEnum 915 = RangeSubselectTag
  toEnum 916 = RangeFunctionTag
  toEnum 917 = RangeTableSampleTag
  toEnum 918 = TypeNameTag
  toEnum 919 = ColumnDefTag
  toEnum 920 = IndexElemTag
  toEnum 921 = ConstraintTag
  toEnum 922 = DefElemTag
  toEnum 923 = RangeTblEntryTag
  toEnum 924 = RangeTblFunctionTag
  toEnum 925 = TableSampleClauseTag
  toEnum 926 = WithCheckOptionTag
  toEnum 927 = SortGroupClauseTag
  toEnum 928 = GroupingSetTag
  toEnum 929 = WindowClauseTag
  toEnum 930 = FuncWithArgsTag
  toEnum 931 = AccessPrivTag
  toEnum 932 = CreateOpClassItemTag
  toEnum 933 = TableLikeClauseTag
  toEnum 934 = FunctionParameterTag
  toEnum 935 = LockingClauseTag
  toEnum 936 = RowMarkClauseTag
  toEnum 937 = XmlSerializeTag
  toEnum 938 = WithClauseTag
  toEnum 939 = InferClauseTag
  toEnum 940 = OnConflictClauseTag
  toEnum 941 = CommonTableExprTag
  toEnum 942 = RoleSpecTag

type Oid = CInt
data ListData = ListData (Ptr Void) CInt Oid
data ListCell = ListCell ListData (Ptr ListCell)
data Lst = Lst CInt CInt (Ptr ListCell) (Ptr ListCell)

-- We won't dereference pointers to Nodes
data CNode = CNode ()

-- extern List *raw_parser(const char *str);
foreign import ccall "./postgres/src/include/parser/parser.h raw_parser"
    c_raw_parser:: CString -> Ptr CNode

-- foreign import capi ".postgres/src/include/nodes/nodes.h nodeTag"
foreign import capi "./queryparser.h my_nodeTag"
    c_nodeTag :: Ptr CNode -> CInt

foreign import capi "./postgres/src/include/nodes/pg_list.h linitial"
    c_linitial:: Ptr CNode -> Ptr CNode

foreign import ccall "./queryparser.h cellData"
    c_celldata:: Ptr CNode -> Ptr CNode

-- foreign import capi "./postgres/src/include/nodes/value.h strVal"
--     c_strVal:: Ptr CNode -> CString
foreign import capi "./queryparser.h my_strVal"
     c_strVal:: Ptr CNode -> CString

visit :: Ptr CNode -> IO ()
visit nd = do
    go 0 nd

go :: Int -> Ptr CNode -> IO ()
go t nd = do
    let tag = c_nodeTag nd
    debug (show tag)
    visit' t nd $ toEnum ( fromIntegral tag )

print' :: Int -> String -> IO ()
print' t s = print $ (replicate t '\t') ++ s

debug :: String -> IO ()
debug s = print $ "***** " ++ s

visit' :: Int -> Ptr CNode -> NodeTag -> IO ()
visit' t nd ListTag = do
    debug "list"
    headCell <- (#{peek List, head} nd)
    loopCell 0 headCell
  where
    loopCell i lc = do
      debug "cell"
      let cellNode = c_celldata lc
      nextCell <- (#{peek ListCell, next} nd)
      case i of
        0 -> go t cellNode
        _ -> do
                print' 0 "."
                go 0 cellNode
      if (nextCell /= nullPtr)
        then loopCell (i+1) nextCell
        else return ()

visit' t nd SelectStmntTag = do
    print' t "SELECT\n"

    targetNode <- (#{peek SelectStmt, targetList} nd)
    debug "target list"
    go (t + 1) targetNode

    fromNode <- (#{peek SelectStmt, fromClause} nd)
    debug "from clause"
    print' t "FROM\n"
    go (t + 1) fromNode
visit' t nd ResTargetTag = do
    debug "resTarget"
    name <- (#{peek ResTarget, name} nd)
    debug $ show (name == nullPtr)
    valNode <- (#{peek ResTarget, val} nd)
    go t valNode
visit' t nd JoinExprTag = do
    debug "joinClause TODO"
    joinType <- (#{peek JoinExpr, jointype} nd) :: (IO CInt)
    debug "join type:"
    debug (show joinType)
    debug "left"
    leftNode <- (#{peek JoinExpr, larg} nd)
    go t leftNode
    print "right"
    rightNode <- (#{peek JoinExpr, rarg} nd)
    go t rightNode
    debug "quals"
    qualNode <- (#{peek JoinExpr, quals} nd)
    go t qualNode
visit' t nd ColumnRefTag = do
    debug "column"
    fieldsNode <- (#{peek ColumnRef, fields} nd)
    go t fieldsNode
visit' t nd StringTag = do
    debug "string"
    str <- peekCString $ c_strVal nd
    print' t str
-- visit' t nd RangeVarTag = do
--     debug "table"
--     schema <- (#{peek RangeVar, schemaname} nd) :: IO CString
--     table <- (#{peek RangeVar, relname} nd) :: IO CString
--     if (schema /= nullPtr)
--       then peekCString schema >>= (print' t)
--       else debug "no schema"
--     if (table /= nullPtr)
--       then peekCString table >>= (print' t)
--       else debug "no table"
--     aliasNode <- (#{peek RangeVar, alias} nd)
--     if (aliasNode /= nullPtr)
--       then go t aliasNode
--       else debug "no alias"
-- visit' t nd A_ExprTag = do
--     debug "expr"
--     nameNode <- (#{peek A_Expr, name} nd)
--     debug "name"
--     go t nameNode
--     debug "left"
--     leftNode <- (#{peek A_Expr, lexpr} nd)
--     go t leftNode
--     debug "right"
--     rightNode <- (#{peek A_Expr, rexpr} nd)
--     go t rightNode


visit' _ _ t = do
    debug "not handled yet"
    debug (show t)

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
    targetCNode <- (#{peek SelectStmt, targetList} nd)
    targetNode <- extractList <$> parse targetCNode

    fromCNode <- (#{peek SelectStmt, fromClause} nd)
    fromNode <- extractList <$> parse fromCNode
    return $ SelectStmnt targetNode fromNode Nothing []

parse' nd ResTargetTag = do
    debug "resTarget"
    cname <- (#{peek ResTarget, name} nd)
    name <- if (cname == nullPtr)
                then return Nothing
                else fmap Just $ peekCString cname
    valNode <- (#{peek ResTarget, val} nd)
    vals <- parse valNode
    -- TODO will this always return a SelectTarget?
    return $ SelectTarget name (extractList vals)

parse' nd ColumnRefTag = do
    fieldsNode <- (#{peek ColumnRef, fields} nd)
    fields <- extractList <$> parse fieldsNode
    -- TODO what else can happen here??
    let fieldStrings = extractString <$> fields
    return $ ColumnRef $ fieldStrings
  where
    extractString (StringNode s) = s
    extractString _ = undefined

parse' nd StringTag = do
    str <- peekCString $ c_strVal nd
    debug str
    return $ StringNode str

parse' nd JoinExprTag = do
    cjoinType <- (#{peek JoinExpr, jointype} nd) :: (IO CInt)
    let joinType = fromIntegral cjoinType
    debug "join type:"
    debug (show joinType)
    leftNode <- (#{peek JoinExpr, larg} nd)
    left <- parse leftNode
    rightNode <- (#{peek JoinExpr, rarg} nd)
    right <- parse rightNode
    qualNode <- (#{peek JoinExpr, quals} nd)
    -- TODO optional?
    qual <- parse qualNode
    -- TODO Alias
    return $ JoinExpr joinType left right (Just qual) Nothing

-- parse' nd RangeVarTag = do
--     debug "table"
--     schema <- (#{peek RangeVar, schemaname} nd) :: IO CString
--     table <- (#{peek RangeVar, relname} nd) :: IO CString
--     if (schema /= nullPtr)
--       then peekCString schema >>= (print' t)
--       else debug "no schema"
--     if (table /= nullPtr)
--       then peekCString table >>= (print' t)
--       else debug "no table"
--     aliasNode <- (#{peek RangeVar, alias} nd)
--     if (aliasNode /= nullPtr)
--       then go t aliasNode
--       else debug "no alias"

parse' _ t = do
    debug "not handled yet"
    debug (show t)
    return UnhandledNode


extractList :: Node -> [Node]
extractList (NodeList nd) = nd
extractList _ = undefined

parseIt = do
    -- Easy
    -- useAsCString (pack "select 1") $ \s ->
    --     c_easy_parse s
    -- Do
    c_MemoryContextInit
    let s1 = "select a.aa as xyz from a join b on a.x = b.y"
    useAsCString (pack s1) $ \s -> do
        let nd = c_raw_parser s
        print s1
        parse nd
        print "done"
    -- Full Parse
    -- useAsCString (pack "select a.aa from a as c join b on c.x = b.y where c.xx = 7") $ \s -> do
    --     let nd = c_raw_parser s
    --     visit nd
    --     print "done"
