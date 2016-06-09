module Types where

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

      -- These are TableColumn
    | SelectTarget (Maybe String) [Node]
    | ColumnRef [String]
    | TableRef
        (Maybe String) -- db
        (Maybe String) -- schema
        (Maybe String) -- table
        (Maybe Node)   -- alias
    | StringNode String
