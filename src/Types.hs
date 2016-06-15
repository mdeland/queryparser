module Types where

import Control.Monad (when)
import qualified Control.Monad.State as State
import qualified Data.Text as T
import qualified Data.List as L
import Data.Maybe
import CApi

data Node =
    UnhandledNode
    | NodeList [Node]
  -- postgres/src/include/nodes/parsenodes.h
    | SelectStmnt
        -- [Node]           -- NULL, list of DISTINCT ON exprs OR
                         -- lcons(NIL,NIL) for all (SELECT DISTINCT)
        -- Maybe IntoClause -- target for SELECT INTO

        -- These are SelectTarget
          [Node]           -- the target list (of ResTarget) */
          [Node]           -- FromClause
          (Maybe Node)     -- WhereClause
          [Node]           -- Group

      -- HavingClause     -- HAVING conditional-expression
      -- [WindowClause]   -- WINDOW window_name AS (...), ... */
      -- [Value]
      -- [SortClause]
      -- LimitOffset
      -- LimitCount
      -- [LockingClause]
      -- WithClause
      -- SetOperation op
      -- all bool
      -- SelectStmnt left
      -- SelectStmnt right
    | JoinExpr
        Int -- Join Type, TODO type better via enum
        Node -- Left
        Node -- Right
        (Maybe Node) -- qualfiers
        (Maybe Node) -- alias

    | FuncCall
        [Node] -- names
        [Node] -- args
        [Node] --- agg order
        (Maybe Node) -- filter
        Bool -- withinGroup
        Bool -- star
        Bool -- distinct
        Bool -- variadic
        -- TODO windowdef
      -- These are TableColumn
    | SelectTarget (Maybe String) Node
    | ColumnRef [Node]
    | TableRef
        (Maybe String) -- db
        (Maybe String) -- schema
        (Maybe String) -- table
        (Maybe String)   -- alias
    | StringNode String
    | ConstInt Int
    | ConstFloat Float
    | ConstString String
    | ConstNull
    | A_Star
    | RangeSubselect
        Bool           -- lateral
        Node           -- subquery
        (Maybe String) -- alias
    | A_Expr
        Int -- type TODO enum this
        Node -- List
        Node -- Left
        Node -- Right
    | Alias
        String -- name
        (Maybe Node)   -- List

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

