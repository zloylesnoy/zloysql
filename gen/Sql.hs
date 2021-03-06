﻿module Sql (
    module Sch,
    module SqlType,

    SqlTextOrParam(..), SqlWithParams,
    getQueryString, getParamNames,
    sqlCreate, sqlDrop, sqlQueries,
    HasSql, getSql
) where

import Data.String.Utils (join)
import Data.List (intercalate)
import qualified Data.Map.Strict as Map

import Sch
import SqlType


-- |Element of SQL query.
data SqlTextOrParam = SqlText String | SqlParam String

-- |SQL query with named parameters.
type SqlWithParams = [SqlTextOrParam]

sql :: String -> SqlWithParams
sql query = [SqlText query]

-- |Concatenates neighbour strings.
sqlSimplify :: SqlWithParams -> SqlWithParams
sqlSimplify xs = foldr f [] xs
  where
    f :: SqlTextOrParam -> SqlWithParams -> SqlWithParams
    f (SqlText s1) (SqlText s2:xs) = SqlText (s1 ++ s2) : xs
    f x xs = x:xs


-- |Get SQL query string with '?' instead of parameters.
getQueryString :: SqlWithParams -> String
getQueryString xs = concat $ map f xs
  where
    f :: SqlTextOrParam -> String
    f (SqlText  s) = s
    f (SqlParam _) = "?"

-- |Get list of parameter names.
getParamNames :: SqlWithParams -> [String]
getParamNames xs = concat $ map f xs
  where
    f :: SqlTextOrParam -> [String]
    f (SqlText  _) = []
    f (SqlParam s) = [s]


sqlExpr :: DialectSQL -> Expression -> SqlWithParams
sqlExpr lang expr = case simplify expr of

    ExprNull  -> sql "NULL"
    ExprTrue  -> sql "(0 = 0)"
    ExprFalse -> sql "(0 = 1)"
    ExprIntegerConst c  -> sql $ if c < 0
        then "(" ++ show c ++ ")"
        else show c
    ExprDoubleConst c -> sql $ if c < 0
        then "(" ++ show c ++ ")"
        else show c
    ExprDecimalConst digits precision -> sql $ show digits -- TODO
    ExprStringConst c -> sql $ escaped lang c

    -- |Unary operator.
    ExprUnary oper e1 -> sql ("(" ++ oper) ++ sqlExpr lang e1 ++ sql ")"

    -- |Binary operator.
    ExprBinary e1 "^" e2 -> let
        xor = if lang == PostgreSQL then "#" else "^"
      in bin e1 xor e2
    ExprBinary e1 oper e2 -> bin e1 oper e2

    ExprParam param -> [SqlParam param]
    ExprField tab fld -> sql $ quotedId lang (getName tab) ++ "." ++ quotedId lang fld
    ExprSelect sel -> sql "(\n" ++ sqlSelect lang sel ++ sql "\n)"
    ExprOuter alias fld t -> sql $ quotedId lang alias ++ "." ++ quotedId lang fld
    ExprJoinUsing flds -> sql $ "USING " ++ join ", " (map (quotedId lang) flds)
    ExprDefault -> sql "DEFAULT"
  where
    bin :: Expression -> String -> Expression -> SqlWithParams
    bin e1 op e2 = sql "(" ++ sqlExpr lang e1 ++ sql (" " ++ op ++ " ") ++ sqlExpr lang e2 ++ sql ")"
    

-- | Returns LIMIT ... OFFSET ... section of the SQL SELECT statement.
sqlLimit :: DialectSQL -> Expression -> Expression -> SqlWithParams

sqlLimit MySQL lim ofs = if (ofs == ExprIntegerConst 0) && (lim == ExprNull) then sql ""
    -- [LIMIT row_count OFFSET offset]
    else sql "\nLIMIT " ++ sqlExpr MicrosoftSQL lim ++ sql "\nOFFSET " ++ sqlExpr MicrosoftSQL ofs

sqlLimit MicrosoftSQL lim ofs = ofset ++ limit
    -- [OFFSET number ROWS [FETCH FIRST number ROWS ONLY]]
  where
    ofset = if (ofs == ExprIntegerConst 0) && (lim == ExprNull) then sql ""
        else sql "\nOFFSET " ++ sqlExpr MicrosoftSQL ofs ++ sql " ROWS"
    limit = if lim == ExprNull then sql ""
        else sql "\nFETCH FIRST " ++ sqlExpr MicrosoftSQL lim ++ sql " ROWS ONLY"

sqlLimit PostgreSQL lim ofs = limit ++ ofset
    -- [ LIMIT number ] [ OFFSET number ]
  where
    limit = if lim == ExprNull then sql ""
        else sql "\nLIMIT " ++ sqlExpr PostgreSQL lim
    ofset = if ofs == ExprIntegerConst 0 then sql ""
        else sql "\nOFFSET " ++ sqlExpr PostgreSQL ofs


sqlOrder :: DialectSQL -> Order -> String
sqlOrder lang (Asc  fld) = quotedId lang fld ++ " ASC"
sqlOrder lang (Desc fld) = quotedId lang fld ++ " DESC"

sqlOrders :: DialectSQL -> String -> [Order] -> String
sqlOrders _ _ []      = ""
sqlOrders lang hdr xs = hdr ++ join ", " (map (sqlOrder lang) xs)


-- | Returns SELECT ... FROM section of the SQL SELECT statement.
sqlSelectResult :: DialectSQL -> Select -> Field -> SqlWithParams
sqlSelectResult lang sel fld = case fldExpr of
    -- Nothing   -> error "Field not found in select'binds"
    Nothing   -> sql $ quotedId lang fldName
    Just expr -> sqlExpr lang expr ++ sql (" AS " ++ quotedId lang fldName)
  where
    fldName = getName fld
    fldExpr = Map.lookup fldName (getBinds sel) -- :: Maybe Expression


sqlOn :: DialectSQL -> Expression -> SqlWithParams
sqlOn _ ExprTrue = sql ""
sqlOn lang expr  = sql " ON " ++ sqlExpr lang expr


-- | Returns FROM ... section of the SQL SELECT statement.
sqlFrom :: DialectSQL -> From -> SqlWithParams

sqlFrom lang (FromAs alias frm) = sqlFrom lang frm ++ sql (" AS " ++ quotedId lang alias)

sqlFrom lang (FromTable tab) = sql $ quotedId lang (getName tab)

sqlFrom lang (CrossJoin f1 f2) = sql "(" ++ sqlFrom lang f1 ++ sql " CROSS JOIN " ++ sqlFrom lang f2 ++ sql ")"

sqlFrom lang (InnerJoin f1 f2 expr) = sql "(" ++ sqlFrom lang f1 ++
    sql " INNER JOIN " ++ sqlFrom lang f2 ++ sqlOn lang expr ++ sql ")"

sqlFrom lang (LeftJoin f1 f2 expr) = sql "(" ++ sqlFrom lang f1 ++
    sql " LEFT JOIN " ++ sqlFrom lang f2 ++ sqlOn lang expr ++ sql ")"

sqlFrom lang (RightJoin f1 f2 expr) = sql "(" ++ sqlFrom lang f1 ++
    sql " RIGHT JOIN " ++ sqlFrom lang f2 ++ sqlOn lang expr ++ sql ")"

sqlFrom lang (FromSelect sel) = sqlSelect lang sel


-- |Returns SQL SELECT statement.
sqlSelect :: DialectSQL -> Select -> SqlWithParams
sqlSelect lang sel = sqlSimplify $
    sql ("SELECT" ++ sDistinct ++ "\n  ") ++ swpResult ++
    sql "\nFROM " ++ swpFrom ++ swpWhere ++
    sql sGroupBy ++ swpHaving ++ sql sOrderBy ++ swpLimit
  where
    sDistinct = if getDistinct sel then " DISTINCT" else ""
    swpFrom   = sqlFrom lang (getFrom sel)
    swpWhere  = let wr = getWhere sel in
        if wr == ExprTrue then sql "" else sql "\nWHERE " ++ sqlExpr lang wr
    swpHaving = let hv = getHaving sel in
        if hv == ExprTrue then sql "" else sql "\nHAVING " ++ sqlExpr lang hv
    sGroupBy  = sqlOrders lang "\nGROUP BY " (getGroupBy sel)
    sOrderBy  = sqlOrders lang "\nORDER BY " (getOrderBy sel)
    swpLimit  = sqlLimit lang (getLimit sel) (getOffset sel)
    swpResult = intercalate (sql ",\n  ") $
        map (sqlSelectResult lang sel) (getFields $ getResult sel)


sqlDefaultValue :: DialectSQL -> Value -> String
sqlDefaultValue lang (StringValue x) = escaped lang x
sqlDefaultValue _ NullValue          = "NULL"
sqlDefaultValue _ (IntValue x)       = show x
sqlDefaultValue _ (DoubleValue x)    = show x
sqlDefaultValue _ (DecimalValue x)   = show x


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
sqlDelete :: DialectSQL -> Delete -> SqlWithParams
sqlDelete lang del = sqlSimplify $ sql ("DELETE FROM " ++ sFrom) ++ swpWhere
  where
    sFrom  = quotedId lang $ getName $ getTable del
    swpWhere = let wr = getWhere del in
        if wr == ExprTrue then sql "" else sql "\nWHERE " ++ sqlExpr lang wr


-- |SQL UPDATE code generator.
sqlUpdate :: DialectSQL -> Update -> SqlWithParams
sqlUpdate lang upd = sqlSimplify $
    sql ("UPDATE " ++ sTable ++ " SET\n  ") ++ swpSets ++ swpWhere
  where
    sTable = quotedId lang $ getName $ getTable upd
    swpWhere = let wr = getWhere upd in
        if wr == ExprTrue then sql "" else sql "\nWHERE " ++ sqlExpr lang wr
    swpSets = intercalate (sql "\n  ") $ map perSet (getUpdateSet upd)

    perSet :: SetExpression -> SqlWithParams
    perSet (SetExpression s ex) = sql (quotedId lang s ++ " = ") ++ sqlExpr lang ex


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
sqlQueries lang sch = map getQueryString swps
  where
    swps :: [SqlWithParams]
    swps = map (sqlSelect lang) (getSelects sch)
        ++ map (sqlDelete lang) (getDeletes sch)
        ++ map (sqlUpdate lang) (getUpdates sch)

class HasSql it where
    getSql :: DialectSQL -> it -> SqlWithParams

instance HasSql Select where
    getSql = sqlSelect

instance HasSql Delete where
    getSql = sqlDelete

instance HasSql Update where
    getSql = sqlUpdate
















