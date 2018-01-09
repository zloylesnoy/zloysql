module Scheme (
    Scheme, scheme,
    SchemeAdd, add, adds,

    getTypes, getRecords, getTables, getIndexes, getForeignKeys,
    getSelects, getDeletes, getUpdates
) where 

import Data.String.Utils (join)

import Common
import Type
import Field
import Record
import Table
import Index
import ForeignKey
import Select
import Delete
import Update


-- |Схема базы данных. Если при построении схемы обнаруживаются
--  ошибки, они сохраняются в списке scheme'errors.
data Scheme = Scheme {
    scheme'name    :: String,
    scheme'comment :: [String],
    scheme'types   :: [Type],
    scheme'records :: [Record],
    scheme'tables  :: [Table],
    scheme'indexes :: [Index],
    scheme'fkeys   :: [ForeignKey],
    scheme'selects :: [Select],
    scheme'deletes :: [Delete],
    scheme'updates :: [Update],
    scheme'errors  :: Errors
}

scheme :: String -> Scheme
scheme s = Scheme{
    scheme'name    = s,
    scheme'comment = [],
    scheme'types   = [],
    scheme'records = [],
    scheme'tables  = [],
    scheme'indexes = [],
    scheme'fkeys   = [],
    scheme'selects = [],
    scheme'deletes = [],
    scheme'updates = [],
    scheme'errors  = []
}

-- |Преобразовать в строку список элементов схемы типа a.
showItems :: Show a => Scheme -> (Scheme -> [a]) -> String -> String
showItems sch items tag = indented s2
  where
    s1 = join ",\n" $ map show (items sch)
    s2 = "\n" ++ tag ++ " = [\n"  ++ indented s1 ++ "\n]\n"

instance Show Scheme where
    show x = "Scheme " ++ show (getName x) ++ " {\n"
        ++ showComment CLang x
        ++ showItems x scheme'types   "Types"
        ++ showItems x scheme'records "Records"
        ++ showItems x scheme'tables  "Tables"
        ++ showItems x scheme'indexes "Indexes"
        ++ showItems x scheme'fkeys   "ForeignKeys"
        ++ showItems x scheme'selects "Selects"
        ++ showItems x scheme'deletes "Deletes"
        ++ showItems x scheme'updates "Updates"
        ++ "Errors = [\n" ++ indented (indented sErrors  ++ "\n]\n")
        ++ "\n}"
      where
        sErrors = join "\n" $ scheme'errors x

instance HasName Scheme where
    name s t = t{ scheme'name = s }
    getName = scheme'name
    getTitle it = "scheme '" ++ scheme'name it ++ "'"

instance HasComment Scheme where
    comment ss t = t{ scheme'comment = ss }
    getComment = scheme'comment

getTypes :: Scheme -> [Type]
getTypes = scheme'types

getRecords :: Scheme -> [Record]
getRecords = scheme'records

getTables :: Scheme -> [Table]
getTables = scheme'tables

getIndexes :: Scheme -> [Index]
getIndexes = scheme'indexes

getForeignKeys :: Scheme -> [ForeignKey]
getForeignKeys = scheme'fkeys

getSelects :: Scheme -> [Select]
getSelects = scheme'selects

getDeletes :: Scheme -> [Delete]
getDeletes = scheme'deletes

getUpdates :: Scheme -> [Update]
getUpdates = scheme'updates


addErrorsToScheme :: Errors -> Scheme -> Scheme
addErrorsToScheme errs sch =
    sch{ scheme'errors = oldErrs ++ errs }
  where
    oldErrs = scheme'errors sch


-- |Добавить определение к списку определений.
--  Если определение с таким именем уже есть и новое определение
--  отличается от старого, значит вернуть ошибку.
addDef :: (Eq def, HasName def) => def -> [def] -> ([def], Errors)
addDef new [] = ([new], [])
addDef new (x:xs)
    | getName new /= getName x = (x:defs, errs)      -- переопределение не обнаружено
    | new /= x                 = (defs, errMsg:errs) -- неправильное переопределение
    | otherwise                = (defs, errs)        -- правильное переопределение
  where
    errMsg = "Different definitions of " ++ getTitle new
    (defs, errs) = addDef new xs

-- |Добавить несколько определений к списку определений.
addDefs :: (Eq def, HasName def) => [def] -> [def] -> ([def], Errors)
addDefs [] old = (old, [])
addDefs (x:xs) old = (defs2, errs1 ++ errs2)
  where
    (defs1, errs1) = addDef x old
    (defs2, errs2) = addDefs xs defs1


-- |То, что можно добавить к схеме базы данных.
class SchemeAdd it where
    add :: it -> Scheme -> Scheme

    adds :: [it] -> Scheme -> Scheme
    adds [] sch = sch
    adds (x:xs) sch = adds xs (add x sch)

instance SchemeAdd Type where
    add tip sch = if null errs
        then sch{ scheme'types = tips }
        else addErrorsToScheme errs sch
      where
        (tips, errs) = addDef tip (scheme'types sch)

instance SchemeAdd Record where
    add rcd sch = if null errs
        then sch{ scheme'records = rcds }
            #adds (map getType (getFields rcd))
        else addErrorsToScheme errs sch
      where
        (rcds, errs) = addDef rcd (scheme'records sch)

instance SchemeAdd Table where
    add tab sch = if null errs
        then sch{ scheme'tables = tabs }
            #add (getRecord tab)
        else addErrorsToScheme errs sch
      where
        (tabs, errs) = addDef tab (scheme'tables sch)

instance SchemeAdd Index where
    add idx sch = if null errs
        then sch{ scheme'indexes = idxs }
            #add (getTable idx)
        else addErrorsToScheme errs sch
      where
        (idxs, errs) = addDef idx (scheme'indexes sch)

instance SchemeAdd ForeignKey where
    add fk sch = if null errs
        then sch{ scheme'fkeys = fks }
            #adds [getChildTable fk, getParentTable fk]
        else addErrorsToScheme errs sch
      where
        (fks, errs) = addDef fk (scheme'fkeys sch)

instance SchemeAdd Select where
    add sel sch = if null errs
        then sch{ scheme'selects = sels }
            #adds (innerRecords sel)
            #adds (innerTables sel)
        else addErrorsToScheme errs sch
      where
        (sels, errs) = addDef sel (scheme'selects sch)

instance SchemeAdd Delete where
    add del sch = if null errs
        then sch{ scheme'deletes = dels }
            #adds (innerRecords del)
            #adds (innerTables del)
        else addErrorsToScheme errs sch
      where
        (dels, errs) = addDef del (scheme'deletes sch)

instance SchemeAdd Update where
    add upd sch = if null errs
        then sch{ scheme'updates = upds }
            #adds (innerRecords upd)
            #adds (innerTables upd)
        else addErrorsToScheme errs sch
      where
        (upds, errs) = addDef upd (scheme'updates sch)


-- |Проверить список элементов схемы.
checkItems :: (HasName a, HasCheck a) => Language -> [a] -> Errors
checkItems lang ls = foldr (\f ss -> ss ++ check lang f) [] ls
    ++ map errmsg (notUniques $ map getName $ ls)
      where
        errmsg = (\s -> "Duplicate name '" ++ s ++ "'.")

instance HasCheck Scheme where
    check lang it = errorIn it $ scheme'errors it
        ++ checkName it
        ++ checkItems lang (scheme'types it)
        ++ checkItems lang (scheme'records it)
        ++ checkItems lang (scheme'tables it)
        ++ checkItems lang (scheme'indexes it)
        ++ checkItems lang (scheme'fkeys it)


















