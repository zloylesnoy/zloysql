module Update (
    Update, update,
    getUpdateSet, set
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
    update'set     :: [SetExpression],
    update'where   :: Expression
}   deriving (Eq, Show)

update :: Table -> Update
update tab = Update{
    update'name    = "update",
    update'comment = [],
    update'params  = record,
    update'table   = tab,
    update'set     = [],
    update'where   = ExprTrue
}

instance ToText Update where
    toText x = "Update " ++ show (getName x) ++ " {\n"
        ++ showComment x
        ++ indent ++ "Params = '" ++ getName (update'params x) ++ "'\n"
        ++ indented ("Table = " ++ show (getName $ update'table x)) ++ "\n"
        ++ indented ("Set = [" ++ sSets) ++ "]\n"
        ++ indented ("Where = " ++ toText (update'where x)) ++ "\n"
        ++ "}"
      where
        sSets = (join "\n" (map toText (update'set x)))

instance HasName Update where
    name s r = r{ update'name = s }
    getName = update'name
    getTitle it = "update '" ++ update'name it ++ "'"

instance HasComment Update where
    comment ss t = t{ update'comment = ss }
    getComment = update'comment

instance HasInnerTables Update where
    innerTables  upd = innerTables (update'where upd) ++
        foldr (++) [update'table upd] (map innerTables $ update'set upd)
    innerRecords upd = innerRecords (update'where upd) ++
        foldr (++) [update'params upd] (map innerRecords $ update'set upd)

instance HasParams Update where
    params pars upd = upd{ update'params = pars }
    getParams = update'params

instance HasWhere Update where
    where_ w upd = upd{ update'where = simplify w }
    getWhere = update'where

instance HasTable Update where
    getTable = update'table


getUpdateSet :: Update -> [SetExpression]
getUpdateSet = update'set

set :: [SetExpression] -> Update -> Update
set ss upd = upd{ update'set = ss }














