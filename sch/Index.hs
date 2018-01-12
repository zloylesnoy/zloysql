module Index (
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


-- |Разновидности индексов. Для начала нужно поддерживать первые две.
data IndexKind
    -- |Неуникальный индекс.
    = NotUniqueIndex

    -- |Уникальный индекс, NULL-значения не допускаются.
    | UniqueIndex

    -- |Уникальный индекс, NULL-значения не допускаются.
    --  Быстрое сравнение на равенство, но не ускоряет операции меньше-больше. 
    --  Поддерживается таблицами на движке MEMORY.
    | HashUniqueIndex

    -- |Индекс для полнотекстового поиска.
    | FullTextIndex

    -- |Индекс по более чем одному измерению.
    | SpatialIndex
    deriving (Eq, Show)

-- |Индекс таблицы.
data Index = Index {
    index'name    :: String,    -- ^ Имя индекса.
    index'comment :: [String],  -- ^ Комментарий к индексу.
    index'kind    :: IndexKind, -- ^ Разновидность индекса.
    index'table   :: Table,     -- ^ Индексируемая таблица
    index'order   :: [Order]    -- ^ Поля индекса.
} deriving (Eq)

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


instance Show Index where
    show x = "Index " ++ show (getName x) ++ " {\n"
        ++ sKind
        ++ sqlComment x
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
        ++ check lang (index'table it)
        ++ checkFields it

checkFields :: Index -> Errors
checkFields idx
    | null names = ["Index has no fields."]
    | null dups  = concat $ map (checkOneField idx) names
    | otherwise  = map dupMsg dups -- есть дублирующиеся поля в индексе
  where
    names = map orderFieldName (getOrder idx)
    dups = notUniques names
    dupMsg = (\s -> "Duplicate '" ++ s ++ "' field in index.")

checkOneField :: Index -> String -> Errors
checkOneField idx fld = case getField fld (index'table idx) of
    Nothing -> ["Field '" ++ fld ++ "' not found in table for index."]
    Just f  -> if getNullable (getType f) && (index'kind idx == UniqueIndex)
        then ["Unique index field '" ++ fld ++ "' must be not null."]
        else []




















