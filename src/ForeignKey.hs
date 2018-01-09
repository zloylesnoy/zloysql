module ForeignKey (
    ForeignKeyAction(..),
    ForeignKey, foreignKey,
    getParentTable, getChildTable, getParentFields, getChildFields,
    onDelete, getOnDelete,
    onUpdate, getOnUpdate
) where

import Data.String.Utils (join)

import Common
import Escape
import Type
import Field
import Table


-- |Действие при удалении или изменении внешнего ключа.
data ForeignKeyAction
    -- |Отложенный запрет в MicrosoftSQL и PostgreSQL.
    --  Немедленный запрет в MySQL, работает как RESTRICT.
    --  PostgreSQL различает RESTRICT и NO ACTION.
    = NoAction

    -- |Все поддерживают.
    | Cascade

    -- |Все поддерживают.
    | SetNull

    -- |Немедленный запрет в MySQL и PostgreSQL.
    --  Не поддерживается MicrosoftSQL.
    | Restrict

    -- |Не поддерживается MySQL.
    | SetDefault

    deriving (Eq)

instance Show ForeignKeyAction where
    show x = case x of
        NoAction   -> "NO ACTION"
        Cascade    -> "CASCADE"
        SetNull    -> "SET NULL"
        Restrict   -> "RESTRICT"
        SetDefault -> "SET DEFAULT"


-- |Внешний ключ.
data ForeignKey = ForeignKey {
    fkey'name     :: String,   -- ^ Имя ограничения.
    fkey'comment  :: [String], -- ^ Комментарий к ограничению.
    fkey'parent   :: Table,    -- ^ Таблица, на которую ссылаются. 
    fkey'child    :: Table,    -- ^ Таблица, которая содержит ссылку.
    fkey'fields   :: [(String, String)], -- ^ Имена полей (parent, child).
    fkey'onDelete :: ForeignKeyAction,   -- ^ Действие при удалении родительской записи.
    fkey'onUpdate :: ForeignKeyAction    -- ^ Действие при изменении родительской записи.
} deriving (Eq)

foreignKey :: Table -> Table -> [(String, String)] -> ForeignKey
foreignKey parent child flds = ForeignKey{
    fkey'name     = "",
    fkey'comment  = [],
    fkey'parent   = parent,
    fkey'child    = child,
    fkey'fields   = flds,
    fkey'onDelete = NoAction,
    fkey'onUpdate = NoAction
}

getParentTable :: ForeignKey -> Table
getParentTable = fkey'parent

getChildTable :: ForeignKey -> Table
getChildTable = fkey'child

getParentFields :: ForeignKey -> [String]
getParentFields fkey = map fst (fkey'fields fkey)

getChildFields :: ForeignKey -> [String]
getChildFields fkey = map snd (fkey'fields fkey)


onDelete :: ForeignKeyAction -> ForeignKey -> ForeignKey
onDelete action fkey = fkey{ fkey'onDelete = action }

getOnDelete :: ForeignKey -> ForeignKeyAction
getOnDelete = fkey'onDelete


onUpdate :: ForeignKeyAction -> ForeignKey -> ForeignKey
onUpdate action fkey = fkey{ fkey'onUpdate = action }

getOnUpdate :: ForeignKey -> ForeignKeyAction
getOnUpdate = fkey'onUpdate


instance Show ForeignKey where
    show x = "ForeignKey " ++ show (getName x) ++ " {\n"
        ++ showComment CLang x
        ++ indent ++ "Parent = " ++ getName (getParentTable x) ++ "\n"
        ++ indent ++ "Child  = " ++ getName (getChildTable x) ++ "\n"
        ++ indented ("Fields = [\n" ++ indented sFields ++ "\n]") ++ "\n"
        ++ indented ("OnDelete = " ++ show (fkey'onDelete x)) ++ "\n"
        ++ indented ("OnUpdate = " ++ show (fkey'onUpdate x)) ++ "\n"
        ++ "}"
      where
        sFields = join ",\n" $ map show (fkey'fields x)

instance HasName ForeignKey where
    name s it = it{ fkey'name = s }
    getName = fkey'name
    getTitle it = "foreign key '" ++ getName it ++ "'"

instance HasComment ForeignKey where
    comment ss it = it{ fkey'comment = ss }
    getComment = fkey'comment

instance HasCheck ForeignKey where
    check lang it = errorIn it $ checkName it
        ++ check lang (getParentTable it)
        ++ check lang (getChildTable it)
        ++ checkFields it

checkFields :: ForeignKey -> Errors
checkFields fkey
    | null parentFields     = ["Foreign key has no fields."]
    | null childFields      = ["Foreign key has no fields."]
    | not (null parentDups) = map dupMsg parentDups -- есть дублирующиеся поля
    | not (null childDups ) = map dupMsg childDups  -- есть дублирующиеся поля
    | otherwise              = concat
        (map (checkTypes fkey) (fkey'fields fkey)) -- совпадают ли типы
  where
    parentFields = map fst (fkey'fields fkey)
    childFields  = map snd (fkey'fields fkey)
    parentDups = notUniques parentFields
    childDups  = notUniques childFields
    dupMsg = (\s -> "Duplicate '" ++ s ++ "' field in foreign key.")

-- Найти тип поля, если такое поле есть.
fkeyFieldType :: ForeignKey -> Table -> String -> Maybe Type
fkeyFieldType fkey tab fieldName = case getField fieldName tab of
    Nothing  -> Nothing
    Just fld -> Just (getType fld #name "x" #comment [])

-- Проверяем типы на равенство.
-- Имена типов и комментарии могут не совпадать.
-- Насчёт nullable не уверен. 
checkTypes :: ForeignKey -> (String, String) -> Errors
checkTypes fkey (par, chd) = case (parT, chdT) of
    (Nothing, _) -> ["Field '" ++ par ++ "' not found in parent table."]
    (_, Nothing) -> ["Field '" ++ chd ++ "' not found in child table."]
    (Just pt, Just ct) -> if pt == ct then []
        else ["Field '" ++ par ++ "' and field '" ++ chd ++ "' has different types."]
  where
    parT = fkeyFieldType fkey (getParentTable fkey) par
    chdT = fkeyFieldType fkey (getChildTable fkey) chd


















