module Const (
    Const, getValue
) where

import Common
import Type
import Value


-- |Constant with name and type.
data Const = Const {
    const'name    :: String,
    const'comment :: [String],
    const'type    :: Type,
    const'value   :: Value
}

constant :: Value -> Type -> Const
constant v t = Const {
    const'name    = "",
    const'comment = [],
    const'type    = t,
    const'value   = v
}

instance HasName Const where
    name s c = c{ const'name = s }
    getName = const'name
    getTitle it = "constant '" ++ const'name it ++ "'"

instance HasComment Const where
    comment ss c = c{ const'comment = ss }
    getComment = const'comment

instance HasType Const where
    getType = const'type


getValue :: Const -> Value
getValue = const'value


instance HasCheck Const where
    check lang it = errorIn it $ checkName it
        ++ check lang (const'type it)
        ++ checkValue (const'type it) (const'value it)























