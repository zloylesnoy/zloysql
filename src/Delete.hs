module Delete (
    Delete, delete, deleteAll, deleteBy,
    sqlDelete
) where

import Select

-- |Запрос DELETE.
data Delete = Delete {
    delete'name    :: String,
    delete'comment :: [String],
    delete'params  :: Record,
    delete'table   :: Table,
    delete'where   :: Expression
}   deriving (Eq)

delete :: Table -> Delete
delete tab = Delete{
    delete'name    = "delete",
    delete'comment = [],
    delete'params  = record "empty" [],
    delete'table   = tab,
    delete'where   = ExprTrue
}

deleteAll :: Table -> Delete
deleteAll tab = (delete tab){
    delete'name    = "deleteAll_" ++ getName tab,
    delete'comment = ["Delete all records from table '" ++ getName tab ++ "'"]
}

deleteBy :: Table -> [String] -> Delete
deleteBy tab [] = deleteAll tab
deleteBy tab flds = delete tab
    #where_ (whereGiven tab flds)
    -- TODO #params

instance Show Delete where
    show x = "Delete " ++ show (getName x) ++ " {\n"
        ++ showComment CLang x
        ++ indent ++ "Params = '" ++ getName (delete'params x) ++ "'\n"
        ++ indented ("Table = " ++ show (getName $ delete'table x)) ++ "\n"
        ++ indented ("Where = " ++ show (delete'where x)) ++ "\n"
        ++ "}"

instance HasName Delete where
    name s r = r{ delete'name = s }
    getName = delete'name
    getTitle it = "delete '" ++ delete'name it ++ "'"

instance HasComment Delete where
    comment ss t = t{ delete'comment = ss }
    getComment = delete'comment

instance HasTables Delete where
    innerTables del = [delete'table del] ++ innerTables (delete'where del)
    innerRecords del = innerRecords (delete'where del)

instance HasParams Delete where
    params pars del = del{ delete'params = pars }
    getParams = delete'params

instance HasWhere Delete where
    where_ w del = del{ delete'where = simplify w }
    getWhere = delete'where


-- |Возвращает оператор SQL DELETE.
sqlDelete :: Language -> Delete -> String
sqlDelete lang del = "DELETE FROM " ++ sFrom ++ sWhere
  where
    sFrom  = quotedId lang (getName $ delete'table del)
    sWhere = let wr = getWhere del in
        if wr == ExprTrue then "" else "\nWHERE " ++ sqlExpr lang wr




















