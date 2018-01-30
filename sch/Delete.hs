module Delete (
    Delete, delete, deleteAll, deleteBy, deleteByKey
) where

import Common
import Record
import Table
import Select


-- |Запрос DELETE.
data Delete = Delete {
    delete'name    :: String,
    delete'comment :: [String],
    delete'params  :: Record,
    delete'table   :: Table,
    delete'where   :: Expression
}   deriving (Eq, Show)

delete :: Table -> Delete
delete tab = Delete{
    delete'name    = "delete",
    delete'comment = [],
    delete'params  = record,
    delete'table   = tab,
    delete'where   = ExprTrue
}

deleteAll :: Table -> Delete
deleteAll tab = (delete tab){
    delete'name    = "DeleteAll_" ++ getName tab,
    delete'comment = ["Delete all records from table '" ++ getName tab ++ "'"]
}

deleteBy :: Table -> [String] -> Delete
deleteBy tab [] = deleteAll tab
deleteBy tab flds = delete tab
    #where_ (whereGiven tab flds)
    #params (selectFromRecord flds $ getRecord tab)

deleteByKey :: Table -> Delete
deleteByKey tab = deleteBy tab (getKey tab)
    #params  (primaryKeyRecord tab)
    #name    ("DeleteByKey_" ++ getName tab)
    #comment ["Delete records from table '" ++ getName tab ++ "' by primary key."]

instance ToText Delete where
    toText x = "Delete " ++ show (getName x) ++ " {\n"
        ++ showComment x
        ++ indent ++ "Params = '" ++ getName (delete'params x) ++ "'\n"
        ++ indented ("Table = " ++ show (getName $ delete'table x)) ++ "\n"
        ++ indented ("Where = " ++ toText (delete'where x)) ++ "\n"
        ++ "}"

instance HasName Delete where
    name s r = r{ delete'name = s }
    getName = delete'name
    getTitle it = "delete '" ++ delete'name it ++ "'"

instance HasComment Delete where
    comment ss t = t{ delete'comment = ss }
    getComment = delete'comment

instance HasTable Delete where
    getTable = delete'table

instance HasInnerTables Delete where
    innerTables  del = [delete'table  del] ++ innerTables  (delete'where del)
    innerRecords del = [delete'params del] ++ innerRecords (delete'where del)

instance HasParams Delete where
    params pars del = del{ delete'params = pars }
    getParams = delete'params

instance HasWhere Delete where
    where_ w del = del{ delete'where = simplify w }
    getWhere = delete'where





















