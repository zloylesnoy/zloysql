module Record (
    module Field,

    Record, record,
    fields, addFields,
    joinField, joinFields, joinRecords,
    selectFromRecord, removeFromRecord
) where

import Data.String.Utils (join)
import Field


data Record = Record {
    record'name    :: String,
    record'comment :: [String],
    record'fields  :: [Field]
} deriving (Eq)

record :: String -> [Field] -> Record
record s flds = Record{
    record'name    = s,
    record'comment = [],
    record'fields  = flds
}

instance Show Record where
    show x = "Record " ++ show (getName x) ++ " {\n"
        ++ showComment CLang x
        ++ indented ("Fields = [\n" ++ indented sFields ++ "\n]")
        ++ "\n}"
      where
        sFields = join ",\n" $ map show (record'fields x)

instance HasName Record where
    name s r = r{ record'name = s }
    getName = record'name
    getTitle it = "record '" ++ record'name it ++ "'"

instance HasComment Record where
    comment ss r = r{ record'comment = ss }
    getComment = record'comment

checkFields :: Language -> Record -> Errors
checkFields lang it = foldr (\f ss -> ss ++ check lang f) [] (record'fields it) ++
    map errMsg (notUniques $ map getName $ record'fields it)
      where
        errMsg = (\s -> "Duplicate field name '" ++ s ++ "'.")

instance HasCheck Record where
    check lang it = errorIn it $ checkName it ++ checkFields lang it

instance HasFields Record where
    getFields = record'fields

fields :: [Field] -> Record -> Record
fields flds r = r{ record'fields = flds }

-- |Добавить поля без проверки на совпадение названий.
addFields :: Record -> [Field] -> Record
addFields r fs = r{ record'fields = old ++ fs } where old = record'fields r

-- |Добавить поле, если поля с таким названием не было в записи.
joinField :: Record -> Field -> Record
joinField r fld = if hasField r (getName fld)
    then r
    else addFields r [fld]

-- |Добавить те поля, названий которых не было в записи.
joinFields :: Record -> [Field] -> Record
joinFields = foldl joinField

-- |Объединить две записи. Если есть совпадающие имена полей, оставить только первое из них.
joinRecords :: Record -> Record -> Record
joinRecords r1 r2 = joinFields r1 (getFields r2)
    #name (getName r1 ++ getName r2)
    // (getName r1 ++ " JOIN " ++ getName r2)

-- |Оставить в записи только поля с именами из списка в порядке, заданном списком.
selectFromRecord :: [String] -> Record -> Record
selectFromRecord names rc = record newName newFields
  where
    filterFieldsByName flds nm = filter (\fld -> getName fld == nm) flds
    newFields = concat $ map (filterFieldsByName $ getFields rc) names
    newName = getName rc ++ "_" ++ concat names

-- |Оставить в записи только поля с именами не из списка.
removeFromRecord :: [String] -> Record -> Record
removeFromRecord flds rc = rc{ record'fields = filter f (getFields rc) }
  where
    f fld = not (elem (getName fld) flds)




















