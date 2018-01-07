module Value (
    module Type,
    module Escape,

    Value(..), checkValue, sqlDefaultValue
) where

import Type
import Escape


-- |Значение.
data Value = NullValue         -- NULL
    | IntValue Integer         -- IntKind, BitsKind
    | DecimalValue Integer Int -- DecimalKind(цифры, число цифр после запятой >= 0)
    | DoubleValue Double       -- FloatKind
    | StringValue String       -- StringKind
    deriving(Eq, Show)

sqlDefaultValue :: Language -> Value -> String
sqlDefaultValue lang (StringValue x) = escaped lang x
sqlDefaultValue _ NullValue = "NULL"
sqlDefaultValue _ (IntValue x) = show x
sqlDefaultValue _ (DoubleValue x) = show x
sqlDefaultValue _ (DecimalValue digs prec) = showDecimal digs prec


-- |Проверка, что значение соответствует типу.
checkValue :: Type -> Value -> Errors
checkValue t v = chVal t v (getKind t)

chVal :: Type -> Value -> Kind -> Errors

chVal t NullValue _ = if getNullable t
    then []
    else ["Null value not match '" ++ getName t ++ "' type."]

chVal _ (DoubleValue v) FloatKind = [] -- TODO: Надо проверить, влезает ли в диапазон
chVal t (DoubleValue v) _ = ["Value of the '" ++ getName t ++ "' can not be float."]

chVal t (StringValue v) StringKind
    | getStrlen t < toInteger (length v) = ["Value of the '" ++ getName t ++ "' is too long."]
    -- TODO: Надо проверить кодировку
    | otherwise = []
chVal t (StringValue v) _ = ["Value of the '" ++ getName t ++ "' can not be string."]

chVal t (DecimalValue v d) DecimalKind
    | d < 0     = ["Value of the '" ++ getName t ++ "' has negative precision."]
    | d > getPrecision t = ["Value of the '" ++ getName t ++ "' has too big precision."]
    | v > maxv  = ["Value of the '" ++ getName t ++ "' decimal type is greater than max value."]
    | v < minv  = ["Value of the '" ++ getName t ++ "' decimal type is lesser than min value."]
    | otherwise = []
  where
    dp = getPrecision t - d -- недостаток точности
    maxv = pow10x (getDigits t + dp) - 1 -- max допустимое значение v
    minv = 0 - maxv
chVal t (DecimalValue v d) _ = ["Value of the '" ++ getName t ++ "' can not be decimal."]

chVal t (IntValue v) IntKind
    | v > getMaxValue t = ["Value " ++ show v ++ " of the '" ++ getName t
                            ++ "' type is greater than maxValue."]
    | v < getMinValue t = ["Value " ++ show v ++ " of the '" ++ getName t
                            ++ "' type is lesser than minValue."]
    | otherwise          = []

chVal t (IntValue v) BitsKind
    | v > getMaxValue t = ["Value " ++ show v ++ " of the '" ++ getName t
                            ++ "' type is greater than maxValue."]
    | v < 0              = ["Value " ++ show v ++ " of the '" ++ getName t
                            ++ "' type is lesser than zero."]
    | otherwise          = []

chVal t (IntValue v) DecimalKind
    | v > maxI  = ["Value " ++ show v ++ " of the '" ++ getName t
                   ++ "' decimal type is greater than max value."]
    | v < minI  = ["Value " ++ show v ++ " of the '" ++ getName t
                   ++ "' decimal type is lesser than min value."]
    | otherwise = []
  where
    maxI = pow10x (max (getDigits t - getPrecision t) 0) - 1
    minI = 0 - maxI

chVal t (IntValue v) FloatKind
    -- Тип t может принимать любое целое значение от minI до maxI.
    -- Но если число v не входит в этот диапазон, оно всё ещё может
    -- быть записано в тип t с точностью хуже 1. Как тут быть ?
    | v > maxI  = ["Value " ++ show v ++ " of the '" ++ getName t
                   ++ "' float type is greater than max value."]
    | v < minI  = ["Value " ++ show v ++ " of the '" ++ getName t
                   ++ "' float type is lesser than min value."]
    | otherwise = []
  where
    maxI = pow2x (getMantissa t) - 1
    minI = 0 - maxI
chVal t (IntValue v) _ = ["Value of the '" ++ getName t ++ "' can not be integer."]






















