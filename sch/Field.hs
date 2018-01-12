module Field (
    Field, field,
    HasFields, getFields, hasField, getField
) where

import Common
import Type


-- |Поле записи.
data Field = Field {
    field'name    :: String,
    field'comment :: [String],
    field'type    :: Type
} deriving (Eq)

-- |Конструктор поля записи.
field :: Type -> Field
field t = Field{
    field'name    = getName t,
    field'comment = [],
    field'type    = t
}

instance Show Field where
    show x = "Field " ++ show (getName x) ++ " {\n"
        ++ sqlComment x
        ++ indent ++ "Type = " ++ getName (field'type x)
        ++ "\n}"

instance HasType Field where
    getType = field'type

instance HasName Field where
    name s t = t{ field'name = s }
    getName = field'name
    getTitle it = "field '" ++ field'name it ++ "'"

instance HasComment Field where
    comment ss t = t{ field'comment = ss }
    getComment = field'comment

instance HasCheck Field where
    check lang it = errorIn it $ checkName it ++ check lang (field'type it)

class HasFields it where
    getFields :: it -> [Field]

    hasField :: it -> String -> Bool
    hasField r name = elem name $ map getName $ getFields r

    getField :: String -> it -> Maybe Field
    getField name r = f (getFields r)
      where
        f []     = Nothing
        f (x:xs) = if name == getName x then Just x else f xs


























