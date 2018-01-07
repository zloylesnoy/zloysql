module Index (
    module Table,

    Index, IndexKind(..),
    index,
    unique, getIndexTable,
    sqlCreateIndex
) where

import Data.String.Utils (join)
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
    index'fields  :: [Order]    -- ^ Поля индекса.
} deriving (Eq)

index :: Table -> [Order] -> Index
index tab orr = Index{
    index'name    = "",
    index'comment = [],
    index'kind    = NotUniqueIndex,
    index'table   = tab,
    index'fields  = orr
}

unique :: Index -> Index
unique idx = idx{ index'kind = UniqueIndex }

getIndexTable :: Index -> Table
getIndexTable = index'table


instance Show Index where
    show x = "Index " ++ show (getName x) ++ " {\n"
        ++ sKind
        ++ showComment CLang x
        ++ indented ("Fields = [\n" ++ indented sFields ++ "\n]") ++ "\n"
        ++ indent ++ "Table = " ++ getName (index'table x) ++ "\n"
        ++ "}"
      where
        sKind = indent ++ show (index'kind x) ++ "\n"
        sFields = join ",\n" $ map show (index'fields x)

instance HasName Index where
    name s it = it{ index'name = s }
    getName = index'name
    getTitle it = "index '" ++ getName it ++ "'"

instance HasComment Index where
    comment ss it = it{ index'comment = ss }
    getComment = index'comment

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
    names = map orderFieldName (index'fields idx)
    dups = notUniques names
    dupMsg = (\s -> "Duplicate '" ++ s ++ "' field in index.")

checkOneField :: Index -> String -> Errors
checkOneField idx fld = case getField fld (index'table idx) of
    Nothing -> ["Field '" ++ fld ++ "' not found in table for index."]
    Just f  -> if getNullable (getType f) && (index'kind idx == UniqueIndex)
        then ["Unique index field '" ++ fld ++ "' must be not null."]
        else []


sqlIndexField :: Language -> Order -> String
sqlIndexField lang (Asc  fld) = quotedId lang fld ++ " ASC"
sqlIndexField lang (Desc fld) = quotedId lang fld ++ " DESC"

-- |Возвращает оператор SQL, который создаёт индекс.
sqlCreateIndex :: Language -> Index -> String
sqlCreateIndex lang idx = create
    ++ quotedId lang (getName idx)
    ++ " ON "
    ++ quotedId lang (getName $ index'table idx)
    ++ " ("
    ++ join ", " fields
    ++ ")"
  where
    create = case index'kind idx of
        NotUniqueIndex -> "CREATE INDEX "
        UniqueIndex    -> "CREATE UNIQUE INDEX "
        _ -> error $ show (index'kind idx) ++ " index not implemented yet."
    fields = map (sqlIndexField lang) (index'fields idx)


















