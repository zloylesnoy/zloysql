module Sql (
    sqlDelete,
    sqlCreate, sqlDrop, sqlQueries
) where

import Data.String.Utils (join)

import Common
import Escape
import Type
import Value
import Field
import Table
import Index
import ForeignKey
import Select
import Delete
import Update
import Scheme
import SqlType


sqlDefaultValue :: Language -> Value -> String
sqlDefaultValue lang (StringValue x) = escaped lang x
sqlDefaultValue _ NullValue = "NULL"
sqlDefaultValue _ (IntValue x) = show x
sqlDefaultValue _ (DoubleValue x) = show x
sqlDefaultValue _ (DecimalValue x) = show x


isAutokey :: Table -> Field -> Bool
isAutokey tab fld = case getAutokey tab of
    Nothing -> False
    Just ak -> ak == getName fld

sqlTableField :: Language -> Table -> Field -> String
sqlTableField lang tab fld = case sqlTypeName lang (getType fld) (isAutokey tab fld) of
    Nothing -> error $ "Can not write SQL for " ++ getTitle fld ++ " in " ++ getTitle tab ++ "."
    Just tn -> indent ++ quotedId lang (getName fld) ++ " " ++ tn ++ defValue ++ ",\n"
  where
    defValue = case getDefaultValue (getName fld) tab of
        Nothing -> ""
        Just dv -> " DEFAULT " ++ sqlDefaultValue lang dv

-- |Возвращает оператор SQL, который создаёт таблицу.
--  Таблица должна быть проверена функцией check.
sqlCreateTable :: Language -> Table -> String
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

-- |Возвращает оператор SQL, который уничтожает таблицу.
sqlDropTable :: Language -> Table -> String
sqlDropTable lang tab = "DROP TABLE " ++ quotedId lang (getName tab)


sqlIndexField :: Language -> Order -> String
sqlIndexField lang (Asc  fld) = quotedId lang fld ++ " ASC"
sqlIndexField lang (Desc fld) = quotedId lang fld ++ " DESC"

-- |Возвращает оператор SQL, который создаёт индекс.
sqlCreateIndex :: Language -> Index -> String
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
    fields = map (sqlIndexField lang) (getOrder idx)


-- |Возвращает оператор SQL, который создаёт внешний ключ.
sqlCreateFKey :: Language -> ForeignKey -> String
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
    ++ show (correctAction lang (getOnDelete fkey))
    ++ "\nON UPDATE "
    ++ show (correctAction lang (getOnUpdate fkey))

-- |Проверяет, какой диалект SQL поддерживает какие ForeignKeyAction.
correctAction :: Language -> ForeignKeyAction -> ForeignKeyAction
correctAction lang act = case (lang, act) of
    (MicrosoftSQL, Restrict) -> NoAction
    (MySQL, SetDefault) -> error "Error in foreign key: SET DEFAULT action not supported in MySQL."
    _ -> act


-- |SQL DELETE code generator.
sqlDelete :: Language -> Delete -> String
sqlDelete lang del = "DELETE FROM " ++ sFrom ++ sWhere
  where
    sFrom  = quotedId lang $ getName $ getTable del
    sWhere = let wr = getWhere del in
        if wr == ExprTrue then "" else "\nWHERE " ++ sqlExpr lang wr


-- |Возвращает оператор SQL UPDATE.
sqlUpdate :: Language -> Update -> String
sqlUpdate lang upd = "UPDATE " ++ sTable ++ " SET\n  " ++ sSet ++ sWhere
  where
    sTable = quotedId lang $ getName $ getTable upd
    sWhere = let wr = getWhere upd in
        if wr == ExprTrue then "" else "\nWHERE " ++ sqlExpr lang wr
    sSet = join "\n  " $ map (sqlExpr lang) (getUpdateSet upd)


-- |Возвращает набор операторов SQL, которые создают все таблицы и прочее.
--  Схема должна быть проверена функцией check.
sqlCreate :: Language -> Scheme -> [String]
sqlCreate lang sch = map (sqlCreateTable lang) (getTables sch)
    ++ map (sqlCreateIndex lang) (getIndexes sch)
    ++ map (sqlCreateFKey  lang) (getForeignKeys sch)


-- |Возвращает набор операторов SQL, которые удаляют все таблицы.
sqlDrop :: Language -> Scheme -> [String]
sqlDrop lang sch = reverse $ map (sqlDropTable lang) (getTables sch)


-- |Возвращает набор операторов SQL, определённых в схеме.
sqlQueries :: Language -> Scheme -> [String]
sqlQueries lang sch = map (sqlSelect lang) (getSelects sch)
    ++ map (sqlDelete lang) (getDeletes sch)
    ++ map (sqlUpdate lang) (getUpdates sch)


















