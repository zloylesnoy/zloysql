module Value (
    Value(..), checkValue
) where

import Data.Scientific (Scientific, toDecimalDigits)

import Common
import Type
import Escape


data Value
    = NullValue
    | IntValue Integer
    | DecimalValue Scientific
    | DoubleValue Double
    | StringValue String
    deriving(Eq, Show)


-- |Is value valid for that type.
checkValue :: Type -> Value -> Errors
checkValue t v = chVal t v (getKind t)

chVal :: Type -> Value -> Kind -> Errors

chVal t NullValue _ = if getNullable t
    then []
    else ["Null value not match '" ++ getName t ++ "' type."]

chVal _ (DoubleValue _) FloatKind = [] -- TODO: Is in diapason?
chVal t (DoubleValue _) _ = ["Value of the '" ++ getName t ++ "' can not be float."]

chVal t (StringValue v) StringKind
    | getStrlen t < toInteger (length v) = ["Value of the '" ++ getName t ++ "' is too long."]
    -- TODO: Check encoding.
    | otherwise = []
chVal t (StringValue _) _ = ["Value of the '" ++ getName t ++ "' can not be string."]

chVal t (DecimalValue sc) DecimalKind
    |scBefore > tBefore = ["Value of the '" ++ getName t ++ "' decimal type is too big."]
    |scAfter > tAfter = ["Value of the '" ++ getName t ++ "' decimal type has too big precision."]
    | otherwise = []
  where
    (ddd, e) = toDecimalDigits (abs sc)
    n = length ddd
    scAfter  = max 0 (n - e)             -- sc has digits after dot
    scDigits = max scAfter (scAfter + e) -- sc has digits
    scBefore = scDigits - scAfter        -- sc has digits before dot
    tAfter   = getPrecision t            -- t  has digits after dot
    tDigits  = getDigits t               -- t  has digits
    tBefore  = tDigits - tAfter          -- t  has digits after dot
{-
    0.1234E-2 = 0.001234 n=4 e=-2 digits=6 after=6 before=0 zeroes=0
    0.1234E+0 = 0.1234   n=4 e=0  digits=4 after=4 before=0 zeroes=0
    0.1234E+2 = 12.34    n=4 e=2  digits=4 after=2 before=2 zeroes=0
    0.1234E+4 = 1234     n=4 e=4  digits=4 after=0 before=4 zeroes=0
    0.1234E+6 = 123400   n=4 e=6  digits=6 after=0 before=6 zeroes=2
-}
chVal t (DecimalValue _) _ = ["Value of the '" ++ getName t ++ "' can not be decimal."]

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
    | v > maxI  = ["Value " ++ show v ++ " of the '" ++ getName t
                   ++ "' float type is greater than max value."]
    | v < minI  = ["Value " ++ show v ++ " of the '" ++ getName t
                   ++ "' float type is lesser than min value."]
    | otherwise = []
  where
    maxI = pow2x (getMantissa t) - 1
    minI = 0 - maxI
chVal t (IntValue _) _ = ["Value of the '" ++ getName t ++ "' can not be integer."]






















