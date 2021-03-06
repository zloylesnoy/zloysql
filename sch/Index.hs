﻿module Index (
    Index, IndexKind(..),
    index, unique,
    getIndexKind, getOrder
) where

import Data.String.Utils (join)

import Common
import Escape
import Type
import Field
import Table


data IndexKind
    = NotUniqueIndex

    -- NULL values are forbidden.
    | UniqueIndex

    -- NULL values are forbidden. Fast ==, !=. Slow < > <= >=.
    | HashUniqueIndex

    -- Not supported yet.
    | FullTextIndex

    -- Not supported yet.
    | SpatialIndex
    deriving (Eq, Show)

-- |Индекс таблицы.
data Index = Index {
    index'name    :: String,
    index'comment :: [String],
    index'kind    :: IndexKind,
    index'table   :: Table,
    index'order   :: [Order]
} deriving (Eq, Show)

index :: Table -> [Order] -> Index
index tab orr = Index{
    index'name    = "",
    index'comment = [],
    index'kind    = NotUniqueIndex,
    index'table   = tab,
    index'order   = orr
}

unique :: Index -> Index
unique idx = idx{ index'kind = UniqueIndex }

getIndexKind :: Index -> IndexKind
getIndexKind = index'kind

getOrder :: Index -> [Order]
getOrder = index'order


instance ToText Index where
    toText x = "Index " ++ show (getName x) ++ " {\n"
        ++ sKind
        ++ showComment x
        ++ indented ("Fields = [\n" ++ indented sFields ++ "\n]") ++ "\n"
        ++ indent ++ "Table = " ++ getName (index'table x) ++ "\n"
        ++ "}"
      where
        sKind = indent ++ show (index'kind x) ++ "\n"
        sFields = join ",\n" $ map show (getOrder x)

instance HasName Index where
    name s it = it{ index'name = s }
    getName = index'name
    getTitle it = "index '" ++ getName it ++ "'"

instance HasComment Index where
    comment ss it = it{ index'comment = ss }
    getComment = index'comment

instance HasTable Index where
    getTable = index'table

instance HasCheck Index where
    check lang it = errorIn it $ checkName it
        ++ checkKind it
        ++ check lang (index'table it)
        ++ checkFields it

checkKind :: Index -> Errors
checkKind idx = case index'kind idx of
    NotUniqueIndex -> []
    UniqueIndex    -> []
    _              -> ["Index kind " ++ show (index'kind idx) ++ " is not supported." ]

checkFields :: Index -> Errors
checkFields idx
    | null names = ["Index has no fields."]
    | null dups  = concat $ map (checkOneField idx) names -- no duplicated fields
    | otherwise  = map dupMsg dups -- there are duplicated fields in the index
  where
    names = map orderFieldName (getOrder idx)
    dups = repeated names
    dupMsg = (\s -> "Duplicate '" ++ s ++ "' field in index.")

checkOneField :: Index -> String -> Errors
checkOneField idx fld = case getField fld (index'table idx) of
    Nothing -> ["Field '" ++ fld ++ "' not found in table for index."]
    Just f  -> if getNullable (getType f) && (index'kind idx == UniqueIndex)
        then ["Unique index field '" ++ fld ++ "' must be not null."]
        else []




















