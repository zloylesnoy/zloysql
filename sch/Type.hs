module Type (
    Encoding(..), Prefer(..), Kind(..), Type,

    typeInt, typeBits,
    typeFloat, typeFloat32, typeFloat64, typeFloat80,
    typeDecimal,
    typeString, typeChar, typeVarChar, typeText,

    getKind,
    getNullable, nullable, maybeNull, notNull,
    getCastTo, castTo,
    getMinValue, minValue,
    getMaxValue, maxValue,
    getDigits, digits,
    getPrecision, precision,
    getMantissa, mantissa,
    getExpBits, expBits,
    getEncoding, encoding,
    getPrefer, prefer,
    getStrlen, strlen,
    bits, includeValue,

    asSignedInteger,
    asUnsignedInteger,
    asFloat, asFloat32, asFloat64,
    asDecimal, asDecimalInt,

    HasType, getType
) where

import Common


-- |Supported character encodings for string type.
data Encoding
    -- |ASCII encoding, code points from 1 to 127.
    = Ascii

    -- |UNICODE UCS-2 encoding, code points from 1 to 0xFFFF.
    --  Surrogate pais not supported.
    | Ucs2

    deriving (Show, Eq)

-- |Preferable string type for database.
data Prefer
    -- |Better use CHAR. If size is big, then VARCHAR.
    --  If VARCHAR is not enough, then use TEXT for MySQL or VARCHAR(MAX) for MS SQL.
    = PreferChar

    -- |Never use CHAR, better use VARCHAR.
    --  If VARCHAR is not enough, then use TEXT for MySQL or VARCHAR(MAX) for MS SQL.
    | PreferVarChar

    -- |TEXT for MySQL, VARCHAR(MAX) for MS SQL, VARCHAR for Postgres
    | PreferText

    deriving (Show, Eq)

-- |Kinds of data types.
data Kind
    -- |Signed integer, binary or decimal. Bitwise operations not supported.
    --  Parameters: type'minValue, type'maxValue.
    = IntKind

    -- |Unigned binary integer type. Bitwise operations supported.
    --  Parameters: type'maxValue.
    | BitsKind

    -- |Signed decimal number with fixed point type.
    --  Parameters: type'digits, type'precision.
    | DecimalKind

    -- |Signed binary number with float point type.
    --  Parameters: type'mantissa, type'expBits.
    | FloatKind
   
    -- |String type.
    --  Parameters: type'encoding, type'prefer, type'strlen.
    | StringKind

    deriving (Show, Eq)

-- |Data type.
data Type = Type {
    type'kind      :: Kind,
    type'name      :: String,
    type'comment   :: [String],
    type'nullable  :: Bool,
    type'castTo    :: Maybe String,

    -- |Only for IntKind.
    --  Mainimum integer value.
    type'minValue  :: Maybe Integer,

    -- |Only for IntKind and BitsKind.
    --  Maximal integer value.
    type'maxValue  :: Maybe Integer,

    -- |Only for DecimalKind.
    --  Decimal digits required.
    type'digits    :: Maybe Int,

    -- |Only for DecimalKind.
    --  Digits after decimal point.
    type'precision :: Maybe Int,

    -- |Only for FloatKind.
    --  Mantissa size in bits. Sign bit included.
    --  Most significant bit (always 1) not included.
    type'mantissa  :: Maybe Int,

    -- |Only for FloatKind.
    --  Exponent size in bits.
    type'expBits   :: Maybe Int,

    -- |Only for StringKind.
    --  Character encoding.
    type'encoding  :: Maybe Encoding,

    -- |Only for StringKind.
    --  Preferable string type for database.
    type'prefer    :: Maybe Prefer,

    -- |Only for StringKind.
    --  Maximum string length in characters.
    type'strlen    :: Maybe Integer
}   deriving (Eq, Show)

instance ToText Type where
    toText x = "Type " ++ show (getName x) ++ " {\n"
        ++ sKind
        ++ showComment x
        ++ sNullable
        ++ sCastTo
        ++ sMinValue
        ++ sMaxValue
        ++ sDigits
        ++ sPrecision
        ++ sMantissa
        ++ sExpBits
        ++ sEncoding
        ++ sPrefer
        ++ sStrlen
        ++ "}"
      where
        sKind = indent ++ show (type'kind x) ++ "\n"
        sNullable = if type'nullable x then indent ++ "maybeNull\n" else ""
        sCastTo = case type'castTo x of
            Nothing -> ""
            Just ss -> indent ++ "castTo = " ++ ss ++ "\n"
        sMinValue = case type'minValue x of
            Nothing -> ""
            Just ss -> indent ++ "minValue = " ++ show ss ++ "\n"
        sMaxValue = case type'maxValue x of
            Nothing -> ""
            Just ss -> indent ++ "maxValue = " ++ show ss ++ "\n"
        sDigits = case type'digits x of
            Nothing -> ""
            Just ss -> indent ++ "digits = " ++ show ss ++ "\n"
        sPrecision = case type'precision x of
            Nothing -> ""
            Just ss -> indent ++ "precision = " ++ show ss ++ "\n"
        sMantissa = case type'mantissa x of
            Nothing -> ""
            Just ss -> indent ++ "mantissa = " ++ show ss ++ "\n"
        sExpBits = case type'expBits x of
            Nothing -> ""
            Just ss -> indent ++ "expBits = " ++ show ss ++ "\n"
        sEncoding = case type'encoding x of
            Nothing -> ""
            Just ss -> indent ++ "encoding = " ++ show ss ++ "\n"
        sPrefer = case type'prefer x of
            Nothing -> ""
            Just ss -> indent ++ "prefer = " ++ show ss ++ "\n"
        sStrlen = case type'strlen x of
            Nothing -> ""
            Just ss -> indent ++ "strlen = " ++ show ss ++ "\n"

-- |Private constructor for Type.
newType :: Kind -> Type
newType k = Type{
    type'kind      = k,
    type'name      = "",
    type'comment   = [],
    type'nullable  = False,
    type'castTo    = Nothing,
    type'minValue  = Nothing,
    type'maxValue  = Nothing,
    type'digits    = Nothing,
    type'precision = Nothing,
    type'mantissa  = Nothing,
    type'expBits   = Nothing,
    type'encoding  = Nothing,
    type'prefer    = Nothing,
    type'strlen    = Nothing
}

-- |Signed integer type, size is minimal available.
typeInt :: Type
typeInt = (newType IntKind) {
    type'minValue = Just 0,
    type'maxValue = Just 0
}

-- |Binary unsigned integer type, size is minimal available.
typeBits :: Type
typeBits = (newType BitsKind) {
    type'maxValue = Just 0
}

-- |Float point type, size is minimal available.
typeFloat :: Type
typeFloat = (newType FloatKind) {
    type'mantissa = Just 0,
    type'expBits  = Just 0
}

-- |Float point type, not less standard 32-bit.
typeFloat32 :: Type
typeFloat32 = typeFloat #mantissa 24 #expBits 8

-- |Float point type, not less standard 64-bit.
typeFloat64 :: Type
typeFloat64 = typeFloat #mantissa 53 #expBits 11

-- |Float point type, not less 80-bit Intel FPU type.
typeFloat80 :: Type
typeFloat80 = typeFloat #mantissa 64 #expBits 15

-- |Decimal type, size is minimal available.
typeDecimal :: Type
typeDecimal = (newType DecimalKind) {
    type'digits    = Just 0,
    type'precision = Just 0
}

-- |String type, size 1 character.
typeString :: Type
typeString = (newType StringKind) {
    type'encoding = Just Ucs2,
    type'prefer   = Just PreferVarChar,
    type'strlen   = Just 1
}

typeChar :: Integer -> Type
typeChar n = typeString #strlen n #prefer PreferChar

typeVarChar :: Integer -> Type
typeVarChar n = typeString #strlen n #prefer PreferVarChar

typeText :: Integer -> Type
typeText n = typeString #strlen n #prefer PreferText


-- |Check type'castTo field, returns [] if OK.
checkCastTo :: Type -> Errors
checkCastTo it = case type'castTo it of
    Just s -> if goodIdWithDots s
        then []
        else ["Bad type name to cast '" ++ s ++ "'."]
    _ -> []

-- |Check type'minValue field, returns [] if OK.
checkMinValue :: Type -> Errors
checkMinValue it = case (type'kind it, type'minValue it) of
    (IntKind, Nothing) -> ["type'minValue not defined for IntKind."]
    (IntKind, Just v) -> if v > 0
        then ["type'minValue > 0 for IntKind."]
        else []
    (_, Nothing) -> []
    (kind, Just _) -> ["type'minValue defined for " ++ show kind ++ "."]

-- |Check type'maxValue field, returns [] if OK.
checkMaxValue :: Type -> Errors
checkMaxValue it = case (type'kind it, type'maxValue it) of
    (IntKind, Nothing) -> ["type'maxValue not defined for IntKind."]
    (IntKind, Just v) -> if v < 0
        then ["type'maxValue < 0 for IntKind."]
        else []
    (BitsKind, Nothing) -> ["type'maxValue not defined for BitsKind."]
    (BitsKind, Just v) -> if v < 0
        then ["type'maxValue < 0 for BitsKind."]
        else []
    (_, Nothing) -> []
    (kind, Just _) -> ["type'maxValue defined for " ++ show kind ++ "."]


-- |Check type'digits field, returns [] if OK.
checkDigits :: Type -> Errors
checkDigits it = case (type'kind it, type'digits it) of
    (DecimalKind, Nothing) -> ["type'digits not defined for DecimalKind."]
    (DecimalKind, Just v) -> if v <= 0
        then ["type'digits <= 0 for DecimalKind."]
        else []
    (_, Nothing) -> []
    (kind, Just _) -> ["type'digits defined for " ++ show kind ++ "."]

-- |Check type'precision field, returns [] if OK.
checkPrecision :: Type -> Errors
checkPrecision it = case (type'kind it, type'precision it) of
    (DecimalKind, Nothing) -> ["type'precision not defined for DecimalKind."]
    (DecimalKind, Just v) -> if v <= 0
        then ["type'precision <= 0 for DecimalKind."]
        else []
    (_, Nothing) -> []
    (kind, Just _) -> ["type'precision defined for " ++ show kind ++ "."]

-- |Check type'digits and type'precision fields, returns [] if OK.
checkDP :: Type -> Errors
checkDP it = case (type'kind it, type'digits it, type'precision it) of
    (DecimalKind, Just d, Just p) -> if p > d
        then ["type'precision > type'digits for DecimalKind."]
        else []
    (_, _, _) -> []

-- |Check type'mantissa field, returns [] if OK.
checkMantissa :: Type -> Errors
checkMantissa it = case (type'kind it, type'mantissa it) of
    (FloatKind, Nothing) -> ["type'mantissa not defined for FloatKind."]
    (FloatKind, Just v) -> if v < 0
        then ["type'mantissa < 0 for FloatKind."]
        else []
    (_, Nothing) -> []
    (kind, Just _) -> ["type'mantissa defined for " ++ show kind ++ "."]

-- |Check type'expBits field, returns [] if OK.
checkExpBits :: Type -> Errors
checkExpBits it = case (type'kind it, type'expBits it) of
    (FloatKind, Nothing) -> ["type'expBits not defined for FloatKind."]
    (FloatKind, Just v) -> if v < 0
        then ["type'expBits < 0 for FloatKind."]
        else []
    (_, Nothing) -> []
    (kind, Just _) -> ["type'expBits defined for " ++ show kind ++ "."]

-- |Check type'encoding field, returns [] if OK.
checkEncoding :: Type -> Errors
checkEncoding it = case (type'kind it, type'encoding it) of
    (StringKind, Nothing) -> ["type'encoding not defined for StringKind."]
    (StringKind, Just _) -> []
    (kind, Just _) -> ["type'encoding defined for " ++ show kind ++ "."]
    (_, _) -> []
   
-- |Check type'prefer field, returns [] if OK.
checkPrefer :: Type -> Errors
checkPrefer it = case (type'kind it, type'prefer it) of
    (StringKind, Nothing) -> ["type'prefer not defined for StringKind."]
    (StringKind, Just _) -> []
    (kind, Just _) -> ["type'prefer defined for " ++ show kind ++ "."]
    (_, _) -> []

-- |Check type'strlen field, returns [] if OK.
checkStrlen :: Type -> Errors
checkStrlen it = case (type'kind it, type'strlen it) of
    (StringKind, Nothing) -> ["type'strlen not defined for StringKind."]
    (StringKind, Just v) -> if v <= 0
        then ["type'strlen <= 0 for StringKind."]
        else []
    (_, Nothing) -> []
    (kind, Just _) -> ["type'strlen defined for " ++ show kind ++ "."]

instance HasCheck Type where
    check _ it = errorIn it $ checkName it
        ++ checkCastTo it
        ++ checkMinValue it
        ++ checkMaxValue it
        ++ checkDigits it
        ++ checkPrecision it
        ++ checkDP it
        ++ checkMantissa it
        ++ checkExpBits it
        ++ checkEncoding it
        ++ checkPrefer it
        ++ checkStrlen it


instance HasName Type where
    getName = type'name
    name v it = it{ type'name = v }
    getTitle it = "type '" ++ type'name it ++ "'"

instance HasComment Type where
    getComment = type'comment
    comment v it = it{ type'comment = v }


-- Is NULL value supported.
getNullable :: Type -> Bool
getNullable = type'nullable

nullable :: Bool -> Type -> Type
nullable v it = it{ type'nullable = v }

maybeNull :: Type -> Type
maybeNull a = a #nullable True

notNull :: Type -> Type
notNull a = a #nullable False


-- Для некоторых типов данных нужно генерировать автоматическую
-- конвертацию в некий специальный тип, например enum.
getCastTo :: Type -> Maybe String
getCastTo = type'castTo

-- Чтобы очистить поле type'castTo, можно использовать #castTo "".
castTo :: String -> Type -> Type
castTo v it = if v == "" then it{ type'castTo = Nothing } else it{ type'castTo = Just v }


-- Перед использованием геттеров лучше проверить тип функцией checkType.

getKind :: Type -> Kind
getKind = type'kind

getMinValue :: Type -> Integer
getMinValue it = case type'minValue it of
    Just v -> v
    Nothing -> error (getTitle it ++ " type supports type'minValue property.")

getMaxValue :: Type -> Integer
getMaxValue it = case type'maxValue it of
    Just v -> v
    Nothing -> error (getTitle it ++ " not supports type'maxValue property.")

getDigits :: Type -> Int
getDigits it = case type'digits it of
    Just v -> v
    Nothing -> error (getTitle it ++ " not supports type'digits property.")

getPrecision :: Type -> Int
getPrecision it = case type'precision it of
    Just v -> v
    Nothing -> error (getTitle it ++ " not supports type'precision property.")

getMantissa :: Type -> Int
getMantissa it = case type'mantissa it of
    Just v -> v
    Nothing -> error (getTitle it ++ " not supports type'mantissa property.")

getExpBits :: Type -> Int
getExpBits it = case type'expBits it of
    Just v -> v
    Nothing -> error (getTitle it ++ " not supports type'expBits property.")

getEncoding :: Type -> Encoding
getEncoding it = case type'encoding it of
    Just v -> v
    Nothing -> error (getTitle it ++ " not supports type'encoding property.")

getPrefer :: Type -> Prefer
getPrefer it = case type'prefer it of
    Just v -> v
    Nothing -> error (getTitle it ++ " not supports type'prefer property.")

getStrlen :: Type -> Integer
getStrlen it = case type'strlen it of
    Just v -> v
    Nothing -> error (getTitle it ++ " not supports type'strlen property.")


-- Сеттеры ничего не проверяют.
-- Проверка будет потом при помощи checkType.

minValue :: Integer -> Type -> Type
minValue v it = it{ type'minValue = Just v }

maxValue :: Integer -> Type -> Type
maxValue v it = it{ type'maxValue = Just v }

digits :: Int -> Type -> Type
digits v it = it{ type'digits = Just v }

precision :: Int -> Type -> Type
precision v it = it{ type'precision = Just v }

mantissa :: Int -> Type -> Type
mantissa v it = it{ type'mantissa = Just v }

expBits :: Int -> Type -> Type
expBits v it = it{ type'expBits = Just v }

encoding :: Encoding -> Type -> Type
encoding v it = it{ type'encoding = Just v }

prefer :: Prefer -> Type -> Type
prefer v it = it{ type'prefer = Just v }

strlen :: Integer -> Type -> Type
strlen v it = it{ type'strlen = Just v }

-- Специальные сеттеры для minValue и maxValue.

bits :: Int -> Type -> Type
bits v it = case type'kind it of
    IntKind -> it # maxValue (pow2x (v - 1) - 1) # minValue (0 - pow2x (v - 1))
    BitsKind -> it # maxValue (pow2x v - 1)
    _ -> error (getTitle it ++ " not supports type'bits property.")

includeValue :: Integer -> Type -> Type
includeValue v it = it # minValue (min v (getMinValue it)) # maxValue (max v (getMaxValue it))

-- Можно ли хранить тип it как двоичное целое со знаком,
-- имеющее bits бит, включая знаковый бит.
asSignedInteger :: Type -> Int -> Bool
asSignedInteger it bits = case type'kind it of
    IntKind  -> (getMinValue it >= minI) && (getMaxValue it <= maxI)
    BitsKind -> (getMaxValue it <= maxI)
    _        -> False
  where
    maxI = pow2x (bits - 1) - 1
    minI = 0 - pow2x (bits - 1)

-- Можно ли хранить тип it как двоичное целое без знака, имеющее bits бит.
asUnsignedInteger :: Type -> Int -> Bool
asUnsignedInteger it bits = case type'kind it of
    IntKind  -> (getMinValue it >= 0) && (getMaxValue it <= maxI)
    BitsKind -> (getMaxValue it <= maxI)
    _        -> False
  where
    maxI = pow2x bits - 1

-- Можно ли хранить тип it как число с плавающей точкой, имеющее
-- мантиссу из mant бит и экспоненту из expo бит.
asFloat :: Type -> Int -> Int -> Bool
asFloat it mant expo = case type'kind it of
    IntKind   -> (getMinValue it >= minI) && (getMaxValue it < maxI)
    FloatKind -> (getMantissa it <= mant) && (getExpBits it <= expo)
    _         -> False
  where
    maxI = pow2x mant - 1
    minI = 0 - maxI

-- Можно ли хранить тип it как 32-битное число с плавающей точкой.
asFloat32 :: Type -> Bool
asFloat32 it = asFloat it 24 8

-- Можно ли хранить тип it как 64-битное число с плавающей точкой.
asFloat64 :: Type -> Bool
asFloat64 it = asFloat it 53 11

-- Можно ли хранить тип it как десятичное с фиксированной точкой со знаком,
-- имеющее dig цифр, из которых prec цифр после запятой.
asDecimal :: Type -> Int -> Int -> Bool
asDecimal it dig prec = case type'kind it of
    IntKind     -> (getMinValue  it >= minI) && (getMaxValue it < maxI)
    DecimalKind -> (getPrecision it <= prec) && (getDigits it - getPrecision it <= dig - prec)
    _           -> False
  where
    maxI = pow10x (max (dig - prec) 0) - 1
    minI = 0 - maxI

-- Можно ли хранить тип it как десятичное целое со знаком.
-- Если да, возвращает требуемое количество десятичных цифр.
-- Если нет, возвращает -1.
asDecimalInt :: Type -> Int
asDecimalInt it = case type'kind it of
    DecimalKind -> if getPrecision it == 0 then getDigits it else 0 - 1
    IntKind     -> let f x = if asDecimal it x 0 then x else f (x + 1) in f 1
    _           -> 0 - 1 -- в том числе BitsKind, потому что требуется поддержка побитовых операций


class HasType a where
    getType :: a -> Type






















