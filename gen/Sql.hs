module Sql (
    module Sch,
    module SqlType,

    sqlCreate, sqlDrop,
    sqlDelete, sqlUpdate, sqlQueries
) where

import Data.String.Utils (join)
import qualified Data.Map.Strict as Map

import Sch
import SqlType


sqlExpr :: DialectSQL -> Expression -> String
sqlExpr lang expr = case simplify expr of

    ExprNull  -> "NULL"
    ExprTrue  -> "(0 = 0)"
    ExprFalse -> "(0 = 1)"
    ExprIntegerConst c  -> if c < 0
        then "(" ++ show c ++ ")"
        else show c
    ExprDoubleConst c   -> if c < 0
        then "(" ++ show c ++ ")"
        else show c
    ExprDecimalConst digits precision -> show digits -- TODO
    ExprStringConst c -> escaped lang c

    -- |Унарный оператор.
    ExprUnary oper e1 -> "(" ++ oper ++ sqlExpr lang e1 ++ ")"

    -- |Бинарный оператор.
    ExprBinary e1 "^" e2 -> let
        xor = if lang == PostgreSQL then "#" else "^"
      in bin e1 xor e2
    ExprBinary e1 oper e2 -> bin e1 oper e2

    -- Используют контекст. TODO
    ExprParam param t -> "?"
    ExprField tab fld -> quotedId lang (getName tab) ++ "." ++ quotedId lang fld
    ExprSelect sel -> "(\n" ++ sqlSelect lang sel ++ "\n)"
    ExprOuter alias fld t -> quotedId lang alias ++ "." ++ quotedId lang fld
    ExprJoinUsing flds -> "USING " ++ join ", " (map (quotedId lang) flds)
    ExprDefault -> "DEFAULT"
    ExprSet e1 e2 -> sqlExpr lang e1 ++ " = " ++ sqlExpr lang e2
  where
    bin = \e1 op e2 ->
        "(" ++ sqlExpr lang e1 ++ " " ++ op ++ " " ++ sqlExpr lang e2 ++ ")"
    

-- | Возвращает секцию LIMIT ... OFFSET ... оператора SQL SELECT.
sqlLimit :: DialectSQL -> Expression -> Expression -> String

sqlLimit MySQL lim ofs = if (ofs == ExprIntegerConst 0) && (lim == ExprNull) then ""
    -- [LIMIT row_count OFFSET offset]
    else "\nLIMIT " ++ sqlExpr MicrosoftSQL lim ++ "\nOFFSET " ++ sqlExpr MicrosoftSQL ofs

sqlLimit MicrosoftSQL lim ofs = ofset ++ limit
    -- [OFFSET число ROWS [FETCH FIRST число ROWS ONLY]]
  where
    ofset = if (ofs == ExprIntegerConst 0) && (lim == ExprNull) then ""
        else "\nOFFSET " ++ sqlExpr MicrosoftSQL ofs ++ " ROWS"
    limit = if lim == ExprNull then ""
        else "\nFETCH FIRST " ++ sqlExpr MicrosoftSQL lim ++ " ROWS ONLY"

sqlLimit PostgreSQL lim ofs = limit ++ ofset
    -- [ LIMIT число ] [ OFFSET число ]
  where
    limit = if lim == ExprNull then ""
        else "\nLIMIT " ++ sqlExpr PostgreSQL lim
    ofset = if ofs == ExprIntegerConst 0 then ""
        else "\nOFFSET " ++ sqlExpr PostgreSQL ofs


sqlOrder :: DialectSQL -> Order -> String
sqlOrder lang (Asc  fld) = quotedId lang fld ++ " ASC"
sqlOrder lang (Desc fld) = quotedId lang fld ++ " DESC"

sqlOrders :: DialectSQL -> String -> [Order] -> String
sqlOrders _ _ []      = ""
sqlOrders lang hdr xs = hdr ++ join ", " (map (sqlOrder lang) xs)


-- | Возвращает секцию SELECT ... FROM оператора SQL SELECT.
sqlSelectResult :: DialectSQL -> Select -> Field -> String
sqlSelectResult lang sel fld = case fldExpr of
    -- Nothing   -> error "Field not found in select'binds"
    Nothing   -> quotedId lang fldName
    Just expr -> (sqlExpr lang expr) ++ " AS " ++ quotedId lang fldName
  where
    fldName = getName fld
    fldExpr = Map.lookup fldName (getBinds sel) -- :: Maybe Expression


sqlOn :: DialectSQL -> Expression -> String
sqlOn _ ExprTrue = ""
sqlOn lang expr  = " ON " ++ sqlExpr lang expr


-- | Возвращает секцию FROM ... оператора SQL SELECT.
sqlFrom :: DialectSQL -> From -> String

sqlFrom lang (FromAs alias frm) = sqlFrom lang frm ++ " AS " ++ quotedId lang alias

sqlFrom lang (FromTable tab) = quotedId lang $ getName tab

sqlFrom lang (CrossJoin f1 f2) = "(" ++ sqlFrom lang f1 ++ " CROSS JOIN " ++ sqlFrom lang f2 ++ ")"

sqlFrom lang (InnerJoin f1 f2 expr) = "(" ++ sqlFrom lang f1 ++
    " INNER JOIN " ++ sqlFrom lang f2 ++ sqlOn lang expr ++ ")"

sqlFrom lang (LeftJoin f1 f2 expr) = "(" ++ sqlFrom lang f1 ++
    " LEFT JOIN " ++ sqlFrom lang f2 ++ sqlOn lang expr ++ ")"

sqlFrom lang (RightJoin f1 f2 expr) = "(" ++ sqlFrom lang f1 ++
    " RIGHT JOIN " ++ sqlFrom lang f2 ++ sqlOn lang expr ++ ")"

sqlFrom lang (FromSelect sel) = sqlSelect lang sel


-- |Возвращает оператор SQL SELECT.
sqlSelect :: DialectSQL -> Select -> String
sqlSelect lang sel = "SELECT" ++ sDistinct ++ "\n  " ++ sResult ++ "\nFROM " ++ sFrom
    ++ sWhere ++ sGroupBy ++ sHaving ++ sOrderBy ++ sLimit
  where
    sDistinct = if getDistinct sel then " DISTINCT" else ""
    sFrom     = sqlFrom lang (getFrom sel)
    sWhere    = let wr = getWhere sel in
        if wr == ExprTrue then "" else "\nWHERE " ++ sqlExpr lang wr
    sHaving   = let hv = getHaving sel in
        if hv == ExprTrue then "" else "\nHAVING " ++ sqlExpr lang hv
    sGroupBy  = sqlOrders lang "\nGROUP BY " (getGroupBy sel)
    sOrderBy  = sqlOrders lang "\nORDER BY " (getOrderBy sel)
    sLimit    = sqlLimit lang (getLimit sel) (getOffset sel)
    sResult   = join ",\n  " $ map (sqlSelectResult lang sel) (getFields $ getResult sel)


sqlDefaultValue :: DialectSQL -> Value -> String
sqlDefaultValue lang (StringValue x) = escaped lang x
sqlDefaultValue _ NullValue = "NULL"
sqlDefaultValue _ (IntValue x) = show x
sqlDefaultValue _ (DoubleValue x) = show x
sqlDefaultValue _ (DecimalValue x) = show x


sqlTableField :: DialectSQL -> Table -> Field -> String
sqlTableField lang tab fld = case sqlTypeName lang (getType fld) isAutokey of
    Nothing -> error $ "Can not write SQL for " ++ getTitle fld ++ " in " ++ getTitle tab ++ "."
    Just tn -> indent ++ quotedId lang (getName fld) ++ " " ++ tn ++ defValue ++ ",\n"
  where
    defValue = case getDefaultValue (getName fld) tab of
        Nothing -> ""
        Just dv -> " DEFAULT " ++ sqlDefaultValue lang dv
    isAutokey = case getAutokey tab of
        Nothing -> False
        Just ak -> ak == getName fld


-- |SQL CREATE TABLE code generator.
sqlCreateTable :: DialectSQL -> Table -> String
sqlCreateTable lang tab = s0 ++ concat ss ++ pk ++ eng
  where
    s0 = "CREATE TABLE "
        ++ quotedId lang (getName tab)
        ++ " (\n"
    ss = map (sqlTableField lang tab) $ getFields tab
    pk = indent
        ++ "PRIMARY KEY ("
        ++ quotedIds lang (getKey tab)
        ++ ")\n"
    eng = if lang == MySQL
        then ") ENGINE = " ++ show (getEngine tab)
        else ")"


-- |SQL DROP TABLE code generator.
sqlDropTable :: DialectSQL -> Table -> String
sqlDropTable lang tab = "DROP TABLE " ++ quotedId lang (getName tab)


-- |SQL CREATE INDEX code generator.
sqlCreateIndex :: DialectSQL -> Index -> String
sqlCreateIndex lang idx = create
    ++ quotedId lang (getName idx)
    ++ " ON "
    ++ quotedId lang (getName $ getTable idx)
    ++ " ("
    ++ join ", " fields
    ++ ")"
  where
    create = case getIndexKind idx of
        NotUniqueIndex -> "CREATE INDEX "
        UniqueIndex    -> "CREATE UNIQUE INDEX "
        _ -> error $ show (getIndexKind idx) ++ " index not implemented yet."

    sqlIndexField :: Order -> String
    sqlIndexField (Asc  fld) = quotedId lang fld ++ " ASC"
    sqlIndexField (Desc fld) = quotedId lang fld ++ " DESC"

    fields = map sqlIndexField (getOrder idx)


-- |SQL FOREIGN KEY code generator.
sqlCreateFKey :: DialectSQL -> ForeignKey -> String
sqlCreateFKey lang fkey = "ALTER TABLE "
    ++ quotedId lang (getName $ getChildTable fkey)
    ++ " ADD CONSTRAINT "
    ++ quotedId lang (getName fkey)
    ++ "\nFOREIGN KEY ("
    ++ quotedIds lang (getChildFields fkey)
    ++ ")\nREFERENCES "
    ++ quotedId lang (getName $ getParentTable fkey)
    ++ " ("
    ++ quotedIds lang (getParentFields fkey)
    ++ ")\nON DELETE "
    ++ show (correct $ getOnDelete fkey)
    ++ "\nON UPDATE "
    ++ show (correct $ getOnUpdate fkey)
  where
    correct :: ForeignKeyAction -> ForeignKeyAction
    correct act = case (lang, act) of
        (MicrosoftSQL, Restrict  ) -> NoAction
        (MySQL       , SetDefault) -> error "Error in foreign key: SET DEFAULT action not supported in MySQL."
        _                          -> act


-- |SQL DELETE code generator.
sqlDelete :: DialectSQL -> Delete -> String
sqlDelete lang del = "DELETE FROM " ++ sFrom ++ sWhere
  where
    sFrom  = quotedId lang $ getName $ getTable del
    sWhere = let wr = getWhere del in
        if wr == ExprTrue then "" else "\nWHERE " ++ sqlExpr lang wr


-- |SQL UPDATE code generator.
sqlUpdate :: DialectSQL -> Update -> String
sqlUpdate lang upd = "UPDATE " ++ sTable ++ " SET\n  " ++ sSet ++ sWhere
  where
    sTable = quotedId lang $ getName $ getTable upd
    sWhere = let wr = getWhere upd in
        if wr == ExprTrue then "" else "\nWHERE " ++ sqlExpr lang wr
    sSet = join "\n  " $ map (sqlExpr lang) (getUpdateSet upd)


-- |Returns sequence of the SQL operators to create all tables and
--  constraints for database scheme. Scheme must be checked.
sqlCreate :: DialectSQL -> Scheme -> [String]
sqlCreate lang sch = map (sqlCreateTable lang) (getTables sch)
    ++ map (sqlCreateIndex lang) (getIndexes sch)
    ++ map (sqlCreateFKey  lang) (getForeignKeys sch)


-- |Returns sequence of the SQL operators to delete all tables and
--  constraints for database scheme. Scheme must be checked.
sqlDrop :: DialectSQL -> Scheme -> [String]
sqlDrop lang sch = reverse $ map (sqlDropTable lang) (getTables sch)


-- |Returns set of the SQL operators defined in database scheme.
sqlQueries :: DialectSQL -> Scheme -> [String]
sqlQueries lang sch = map (sqlSelect lang) (getSelects sch)
    ++ map (sqlDelete lang) (getDeletes sch)
    ++ map (sqlUpdate lang) (getUpdates sch)


















