module Update (
    Update, update,
    getUpdateSet
) where

import Data.String.Utils (join)

import Common
import Escape
import Record
import Table
import Select


-- |Запрос UPDATE.
data Update = Update {
    update'name    :: String,
    update'comment :: [String],
    update'params  :: Record,
    update'table   :: Table,
    update'set     :: [Expression],
    update'where   :: Expression
}   deriving (Eq)

update :: Table -> Update
update tab = Update{
    update'name    = "update",
    update'comment = [],
    update'params  = record,
    update'table   = tab,
    update'set     = [],
    update'where   = ExprTrue
}

instance Show Update where
    show x = "Update " ++ show (getName x) ++ " {\n"
        ++ sqlComment x
        ++ indent ++ "Params = '" ++ getName (update'params x) ++ "'\n"
        ++ indented ("Table = " ++ show (getName $ update'table x)) ++ "\n"
        ++ indented ("Set = [" ++ sSets) ++ "]\n"
        ++ indented ("Where = " ++ show (update'where x)) ++ "\n"
        ++ "}"
      where
        sSets = (join "\n" (map show (update'set x)))

instance HasName Update where
    name s r = r{ update'name = s }
    getName = update'name
    getTitle it = "update '" ++ update'name it ++ "'"

instance HasComment Update where
    comment ss t = t{ update'comment = ss }
    getComment = update'comment

instance HasTables Update where
    innerTables upd = [update'table upd] ++ innerTables (update'where upd)
    innerRecords upd = innerRecords (update'where upd)

instance HasParams Update where
    params pars upd = upd{ update'params = pars }
    getParams = update'params

instance HasWhere Update where
    where_ w upd = upd{ update'where = simplify w }
    getWhere = update'where

instance HasTable Update where
    getTable = update'table


getUpdateSet :: Update -> [Expression]
getUpdateSet = update'set

















