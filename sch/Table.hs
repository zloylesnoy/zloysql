module Table (
    Table, table,
    getRecord,
    key, getKey,
    autokey, hasAutokey, getAutokey,
    getDefaultValue, removeDefaultValue,
    defaultNull, defaultInt, defaultDecimal, defaultDouble, defaultString,
    MyEngine(..), engine, getEngine,
    HasTable, getTable,

    selectedRecord, primaryKeyRecord
) where

import qualified Data.Map.Strict as Map
import Data.Scientific (Scientific)
import Data.String.Utils (join)

import Common
import Type
import Escape
import Field
import Record
import Value


data MyEngine = MYISAM | INNODB | MEMORY deriving (Show, Eq)

-- |Таблица.
data Table = Table {
    table'name    :: String,
    table'comment :: [String],
    table'record  :: Record,
    table'key     :: [String], -- Primary key.
    table'autokey :: Bool,     -- Primary key is one autoincremented column.
    table'engine  :: MyEngine, -- Only for MySQL.
    table'defs    :: Map.Map String Value -- Default values.
} deriving (Eq, Show)

-- |Конструктор таблицы. Имя записи становится именем таблицы.
table :: Record -> Table
table r = Table {
    table'name    = getName r,
    table'comment = [],
    table'record  = r,
    table'key     = [],
    table'autokey = False,
    table'engine  = INNODB,
    table'defs    = Map.empty
}

instance ToText Table where
    toText x = "Table " ++ show (getName x) ++ " {\n"
        ++ showComment x
        ++ indent ++ k ++ show (table'key x) ++ "\n"
        ++ indent ++ "Engine = " ++ show (table'engine x) ++ "\n"
        ++ indented ("Defaults = [" ++ sDefs) ++ "]\n"
        ++ indented ("Record = " ++ getName (getRecord x))
        ++ "\n}"
      where
        k = if table'autokey x then "Autokey = " else "Key = "
        sDefs = if Map.null (table'defs x) then ""
            else "\n" ++ indented sList ++ "\n"
        sList = join ",\n" $ map show $ Map.toList (table'defs x)
        

instance HasName Table where
    name s t = t{ table'name = s }
    getName = table'name
    getTitle it = "table '" ++ table'name it ++ "'"

instance HasComment Table where
    comment ss t = t{ table'comment = ss }
    getComment = table'comment

instance HasFields Table where
    getFields = getFields . table'record


getRecord :: Table -> Record
getRecord = table'record

key :: [String] -> Table -> Table
key k t = t{ table'key = k, table'autokey = False }

getKey :: Table -> [String]
getKey = table'key


autokey :: String -> Table -> Table
autokey k t = t{ table'key = [k], table'autokey = True }

hasAutokey :: Table -> Bool
hasAutokey = table'autokey

getAutokey :: Table -> Maybe String
getAutokey t = if table'autokey t then Just (head $ table'key t) else Nothing


engine :: MyEngine -> Table -> Table
engine e t = t{ table'engine = e }

getEngine :: Table -> MyEngine
getEngine = table'engine


-- |Установить значение по умолчанию.
setDefaultValue :: (String, Value) -> Table -> Table
setDefaultValue (name, v) t = t{ table'defs = Map.insert name v $ table'defs t }

-- |Установить значение NULL по умолчанию.
defaultNull :: String -> (Table -> Table)
defaultNull s = setDefaultValue (s, NullValue)

-- |Установить целое значение по умолчанию.
defaultInt :: String  -- ^ Имя поля.
           -> Integer -- ^ Значение по умолчанию.
           -> (Table -> Table)
defaultInt s v = setDefaultValue (s, IntValue v)

-- |Установить значение с фиксированной точкой по умолчанию.
defaultDecimal :: String  -- ^ Имя поля.
               -> Scientific
               -> (Table -> Table)
defaultDecimal s v = setDefaultValue (s, DecimalValue v)

-- |Установить значение с плавающей точкой по умолчанию.
defaultDouble :: String -- ^ Имя поля.
              -> Double -- ^ Значение по умолчанию.
              -> (Table -> Table)
defaultDouble s v = setDefaultValue (s, DoubleValue v)

-- |Установить строковое значение по умолчанию.
defaultString :: String -- ^ Имя поля.
              -> String -- ^ Значение по умолчанию.
              -> (Table -> Table)
defaultString s v = setDefaultValue (s, StringValue v)

-- |Получить значение по умолчанию.
getDefaultValue :: String -> Table -> Maybe Value
getDefaultValue name t = Map.lookup name (table'defs t)

-- |Убрать значение по умолчанию.
removeDefaultValue :: String -> Table -> Table
removeDefaultValue name t = t{ table'defs = Map.delete name $ table'defs t }


-- |Проверка таблицы, возвращает список ошибок.
instance HasCheck Table where
    check lang it = errorIn it $ checkName it
        ++ check lang (table'record it)
        ++ checkKey it
        ++ checkAutokey it
        ++ checkDefs it
        ++ checkAutoDef it

checkKeyField :: Table -> String -> Errors
checkKeyField tab fld = case getField fld tab of
    Nothing -> ["Field '" ++ fld ++ "' not found in table for primary key."]
    Just f  -> if getNullable (getType f)
        then ["Primary key field '" ++ fld ++ "' must be not null."]
        else []

checkKey :: Table -> Errors
checkKey t
    | null key  = ["Table has empty primary key."]
    | null dups = concat $ map (checkKeyField t) key
    | otherwise = map dupmsg dups -- есть дублирующиеся поля в primary key
  where
    key = table'key t
    dups = notUniques key
    dupmsg = (\s -> "Duplicate '" ++ s ++ "' field in primary key.")

checkAutokey :: Table -> Errors
checkAutokey t
    | not (table'autokey t)     = [] -- не задан autokey
    | length (table'key t) /= 1 = ["Table has multicolumn autokey."]
    | otherwise                 = case getField (head (table'key t)) (table'record t) of
        Nothing -> [] -- ошибка есть, но её выдаст checkKey
        Just f  -> if getKind (getType f) == IntKind
            then [] -- OK
            else ["Autokey of the table is not IntKind."]

-- |Проверить одно значение по умолчанию.
checkOneDef :: String -> Value -> Table -> Errors
checkOneDef name value t = case getField name (table'record t) of
    Nothing -> ["Table has not column '" ++ name ++ "' but has default value."]
    Just f  -> checkValue (getType f) value

checkDefs :: Table -> Errors
checkDefs t = Map.foldrWithKey
    (\s v errors -> checkOneDef s v t ++ errors)
    [] (table'defs t)

-- |Проверить, что autokey не имеет значения по умолчанию.
checkAutoDef :: Table -> Errors
checkAutoDef t = if not (table'autokey t)
    then [] -- не задан autokey
    else concat (map f (table'key t))
      where
        f name = if Map.member name (table'defs t)
            then ["Table has autokey '" ++ name ++ "' with default value."]
            else []

-- |Возвращает запись, в которую помещается выбранный набор полей таблицы.
selectedRecord :: Table -> [String] -> Record
selectedRecord tab flds = record
    #name (getName tab ++ "_" ++ concat flds)
    #fields (filter isSelected $ getFields tab)
  where
    isSelected fld = elem (getName fld) flds

-- |Возвращает запись, в которую помещается первичный ключ таблицы.
primaryKeyRecord :: Table -> Record
primaryKeyRecord tab = selectedRecord tab (getKey tab) #name (getName tab ++ "_Key")

class HasTable it where
    getTable :: it -> Table
























