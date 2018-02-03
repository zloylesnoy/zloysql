module Record (
    Record, record,
    fields, addFields,
    joinField, joinRecords,
    selectFromRecord, removeFromRecord
) where

import Data.String.Utils (join)

import Common
import Field


data Record = Record {
    record'name    :: String,
    record'comment :: [String],
    record'fields  :: [Field]
} deriving (Eq, Show)

record :: Record
record = Record {
    record'name    = "EmptyRecord",
    record'comment = [],
    record'fields  = []
}

instance ToText Record where
    toText x = "Record " ++ show (getName x) ++ " {\n"
        ++ showComment x
        ++ indented ("Fields = [\n" ++ indented sFields ++ "\n]")
        ++ "\n}"
      where
        sFields = join ",\n" $ map toText (record'fields x)

instance HasName Record where
    name s r = r{ record'name = s }
    getName = record'name
    getTitle it = "record '" ++ record'name it ++ "'"

instance HasComment Record where
    comment ss r = r{ record'comment = ss }
    getComment = record'comment

checkFields :: DialectSQL -> Record -> Errors
checkFields lang it = map errMsg (repeated $ map getName $ record'fields it) ++
    concat (map (check lang) $ record'fields it)
  where
    errMsg = (\s -> "Duplicate field name '" ++ s ++ "'.")

instance HasCheck Record where
    check lang it = errorIn it $ checkName it ++ checkFields lang it

instance HasFields Record where
    getFields = record'fields

fields :: [Field] -> Record -> Record
fields flds r = r{ record'fields = flds }

-- |Add fields, names not checked.
addFields :: [Field] -> Record -> Record
addFields fs r = r{ record'fields = old ++ fs } where old = record'fields r

-- |Add field only if name is unique.
joinField :: Record -> Field -> Record
joinField r fld = if hasField r (getName fld)
    then r
    else addFields [fld] r

-- |Join two records. If there are matching field names, leave only the first one.
joinRecords :: Record -> Record -> Record
joinRecords r1 r2 = foldl joinField r1 (getFields r2)
    #name (getName r1 ++ getName r2)
    // (getName r1 ++ " JOIN " ++ getName r2)

-- |Leave in the record only fields with names from the list in the order given by the list.
selectFromRecord :: [String] -> Record -> Record
selectFromRecord names rc = record
    #name   (getName rc ++ "_" ++ concat names)
    #fields (selectByNames (getFields rc) names)

-- |Keep in the records only fields with names not from the list.
removeFromRecord :: [String] -> Record -> Record
removeFromRecord flds rc = rc{ record'fields = filter f (getFields rc) }
  where
    f fld = not (elem (getName fld) flds)




















