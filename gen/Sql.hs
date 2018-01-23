module Sql (
    module Sch,
    module SqlType,

    sqlCreate, sqlDrop,
    sqlDelete, sqlUpdate, sqlQueries
) where

import Data.String.Utils (join)

import Sch
import SqlType


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


















