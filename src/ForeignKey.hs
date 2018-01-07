module ForeignKey (
    module Table,

    ForeignKeyAction(..),
    ForeignKey, foreignKey,
    getParentTable, getChildTable,
    onDelete, onUpdate,

    sqlCreateFKey
) where

import Data.String.Utils (join)
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


onDelete :: ForeignKeyAction -> ForeignKey -> ForeignKey
onDelete action fkey = fkey{ fkey'onDelete = action }

onUpdate :: ForeignKeyAction -> ForeignKey -> ForeignKey
onUpdate action fkey = fkey{ fkey'onUpdate = action }

instance Show ForeignKey where
    show x = "ForeignKey " ++ show (getName x) ++ " {\n"
        ++ showComment CLang x
        ++ indent ++ "Parent = " ++ getName (fkey'parent x) ++ "\n"
        ++ indent ++ "Child  = " ++ getName (fkey'child x) ++ "\n"
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
        ++ check lang (fkey'parent it)
        ++ check lang (fkey'child it)
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
    parT = fkeyFieldType fkey (fkey'parent fkey) par
    chdT = fkeyFieldType fkey (fkey'child  fkey) chd


-- |Возвращает оператор SQL, который создаёт внешний ключ.
sqlCreateFKey :: Language -> ForeignKey -> String
sqlCreateFKey lang fkey = "ALTER TABLE "
    ++ quotedId lang (getName $ fkey'child fkey)
    ++ " ADD CONSTRAINT "
    ++ quotedId lang (getName fkey)
    ++ "\nFOREIGN KEY ("
    ++ quotedIds lang childFields
    ++ ")\nREFERENCES "
    ++ quotedId lang (getName $ fkey'parent fkey)
    ++ " ("
    ++ quotedIds lang parentFields
    ++ ")\nON DELETE "
    ++ show (correctAction lang (fkey'onDelete fkey))
    ++ "\nON UPDATE "
    ++ show (correctAction lang (fkey'onUpdate fkey))
  where
    childFields  = map snd (fkey'fields fkey)
    parentFields = map fst (fkey'fields fkey)

-- |Проверяет, какой диалект SQL поддерживает какие ForeignKeyAction.
correctAction :: Language -> ForeignKeyAction -> ForeignKeyAction
correctAction lang act = case (lang, act) of
    (MicrosoftSQL, Restrict) -> NoAction
    (MySQL, SetDefault) -> error "Error in foreign key: SET DEFAULT action not supported in MySQL."
    _ -> act















