module Insert (
    Insert, insert,
    into, getInto, values, getData, insertSelect
) where

import Common
import Record
import Table
import Select


data InsertData
    -- |Common insert SQL operator.
    --  INSERT INTO table (col, ..,col) VALUES (expr, ..,expr), (expr, ..,expr)
    = InsertValues [[Expression]]

    -- |Go function has array of record as an input.
    --  SQL operator has not parameters. 
    | MultiInsert

    -- |Insert from select.
    --  INSERT INTO table (col, ..,col) SELECT ...
    | InsertSelect Select

    deriving (Eq, Show)

instance ToText InsertData where
    toText (InsertValues ess) = "InsertValues {\n"
        ++ indented (concat $ map f1 ess)
        ++ "\n}\n"
      where
        f1 :: [Expression] -> String
        f1 es = "[\n" ++ indented (concat $ map f2 es) ++ "]\n"
        f2 :: Expression -> String
        f2 e = toText e ++ "\n"
    toText MultiInsert = "MultiInsert\n"
    toText (InsertSelect sel) = "InsertSelect {\n" ++ indented (toText sel) ++ "\n}\n"


instance HasInnerTables InsertData where
    innerTables (InsertSelect sel) = innerTables sel
    innerTables (InsertValues es) = concat $ concat $ map (map innerTables) es
    innerTables MultiInsert = []

    innerRecords (InsertSelect sel) = innerRecords sel
    innerRecords (InsertValues es) = concat $ concat $ map (map innerRecords) es
    innerRecords MultiInsert = []


-- |SQL INSERT query.
data Insert = Insert {
    insert'name    :: String,
    insert'comment :: [String],
    insert'params  :: Record,
    insert'table   :: Table,
    insert'into    :: [String],
    insert'data    :: InsertData
}   deriving (Eq, Show)

insert :: Table -> [String] -> Insert
insert tab cols = Insert{
    insert'name    = "insert",
    insert'comment = [],
    insert'params  = record,
    insert'table   = tab,
    insert'into    = cols,
    insert'data    = MultiInsert
}

instance HasName Insert where
    name s r = r{ insert'name = s }
    getName = insert'name
    getTitle it = "insert '" ++ insert'name it ++ "'"

instance HasComment Insert where
    comment ss t = t{ insert'comment = ss }
    getComment = insert'comment

instance HasParams Insert where
    params pars ins = ins{ insert'params = pars }
    getParams = insert'params

instance HasTable Insert where
    getTable = insert'table

instance HasInnerTables Insert where
    innerTables  ins = [insert'table  ins] ++ innerTables  (insert'data ins)
    innerRecords ins = [insert'params ins] ++ innerRecords (insert'data ins)

into :: [String] -> Insert -> Insert
into cols ins = ins{ insert'into = cols }

getInto :: Insert -> [String]
getInto = insert'into

values :: [[Expression]] -> Insert -> Insert
values vss ins = ins{ insert'data = InsertValues vss }

getData :: Insert -> InsertData
getData = insert'data

insertSelect :: Select -> Insert -> Insert
insertSelect sel ins = ins{ insert'data = InsertSelect sel }

instance ToText Insert where
    toText x = "Insert " ++ show (getName x) ++ " {\n"
        ++ showComment x
        ++ indent ++ "Params = '" ++ getName (getParams x) ++ "'\n"
        ++ indented ("Table = " ++ show (getName $ getTable x)) ++ "\n"
        ++ indent ++ "Into = '" ++ show (getInto x) ++ "\n"
        ++ indented (toText $ getData x)
        ++ "}"















