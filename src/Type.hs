module Type (
    module Common,

    Encoding(..), Prefer(..), Kind(..), Type,

    typeInt, typeInt16, typeInt32, typeInt64,
    typeBits, typeBit, typeByte,
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
    asFloat, asFloat32, asFloat64, asFloat80,
    asDecimal, asDecimalInt,

    HasType, getType
) where

import Common


-- |Character encodings for string type.
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
    -- |Signed integer, binary or decimal. Per-bit operation not supported. Parameters:
    --    * minValue - Требуемое минимальное значение.
    --    * maxValue - Требуемое максимальное значение.
    = IntKind

    -- |Двоичное целое без знака, поддерживает и арифметические и побитовые операции.
    --  Параметры:
    --    * maxValue - Требуемое максимальное значение.
    | BitsKind

    -- |Десятичное с фиксированной точкой, со знаком. Параметры:
    --    * digits    - Требуемое число десятичных цифр всего.
    --    * precision - Требуемое число десятичных цифр после запятой, больше 0.
    | DecimalKind

    -- |Двоичное с плавающей точкой со знаком. Параметры:
    --    * mantissa - Требуемый размер мантиссы в битах, включая знаковый бит.
    --                 Не считаем старший бит мантиссы, который всегда равен 1 и не хранится.
    --    * expBits  - Требуемый размер экспоненты в битах.
    | FloatKind
   
    -- |Строка. Параметры:
    --    * encoding - Кодировка символов.
    --    * prefer   - Предпочитаемый строковый тип для базы данных.
    --    * strlen   - Длина строки в символах.
    | StringKind

    deriving (Show, Eq)

-- |Описание типа данных. Поля не экспортируются. Часть полей имеют
--  смысл только при некоторых значениях поля type'kind, указанному в комментариях.
data Type = Type {
    type'kind      :: Kind,
    type'name      :: String,
    type'comment   :: [String],
    type'nullable  :: Bool,
    type'castTo    :: Maybe String,
    type'minValue  :: Maybe Integer,  -- IntKind
    type'maxValue  :: Maybe Integer,  -- IntKind, BitsKind
    type'digits    :: Maybe Int,      -- DecimalKind
    type'precision :: Maybe Int,      -- DecimalKind
    type'mantissa  :: Maybe Int,      -- FloatKind
    type'expBits   :: Maybe Int,      -- FloatKind
    type'encoding  :: Maybe Encoding, -- StringKind
    type'prefer    :: Maybe Prefer,   -- StringKind
    type'strlen    :: Maybe Integer   -- StringKind
}   deriving (Eq)

instance Show Type where
    show x = "Type " ++ show (getName x) ++ " {\n"
        ++ sKind
        ++ showComment CLang x
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

-- |Создать заготовку для типа.
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

-- |Создать тип целое со знаком минимального размера.
typeInt :: Type
typeInt = (newType IntKind) {
    type'minValue = Just 0,
    type'maxValue = Just 0
}

-- |Создать тип целое 16-битное со знаком.
typeInt16 :: String -- ^ Имя типа.
          -> Type
typeInt16 s = typeInt
    #name s
    #bits 16

-- |Создать тип целое 32-битное со знаком.
typeInt32 :: String -- ^ Имя типа.
          -> Type
typeInt32 s = typeInt
    #name s
    #bits 32

-- |Создать тип целое 64-битное со знаком.
typeInt64 :: String -- ^ Имя типа.
          -> Type
typeInt64 s = typeInt
    #name s
    #bits 64

-- |Создать тип двоичное целое без знака минимального размера.
typeBits :: Type
typeBits = (newType BitsKind) {
    type'maxValue = Just 0
}

-- |Создать однобитовый тип без знака.
typeBit :: String -- ^ Имя типа.
        -> Type
typeBit s = typeBits
    #name s
    #bits 1

-- |Создать однобайтовый тип без знака.
typeByte :: String -- ^ Имя типа.
         -> Type
typeByte s = typeBits
    #name s
    #bits 8

-- |Создать тип с плавающей точкой минимального размера.
typeFloat :: Type
typeFloat = (newType FloatKind) {
    type'mantissa = Just 0,
    type'expBits  = Just 0
}

-- |Создать тип с плавающей точкой не меньше float, 32 бит.
typeFloat32 :: String -- ^ Имя типа.
            -> Type
typeFloat32 s = typeFloat
    #name s
    #mantissa 24
    #expBits 8

-- |Создать тип с плавающей точкой не меньше double, 64 бит.
typeFloat64 :: String -- ^ Имя типа.
            -> Type
typeFloat64 s = typeFloat
    #name s
    #mantissa 53
    #expBits 11

-- |Создать тип с плавающей точкой не меньше long double, 80 бит.
typeFloat80 :: String -- ^ Имя типа.
            -> Type
typeFloat80 s = typeFloat
    #name s
    #mantissa 64
    #expBits 15

-- |Создать тип с фиксированной точкой минимального размера.
typeDecimal :: Type
typeDecimal = (newType FloatKind) {
    type'digits    = Just 0,
    type'precision = Just 0
}

-- |Создать строковый тип.
typeString :: Type
typeString = (newType StringKind) {
    type'encoding = Just Ucs2,
    type'prefer   = Just PreferVarChar,
    type'strlen   = Just 1
}

typeChar :: Integer -- ^ Max длина в символах.
         -> String  -- ^ Имя типа.
         -> Type
typeChar n s = typeString
    #strlen n
    #name s
    #prefer PreferChar

typeVarChar :: Integer -- ^ Max длина в символах.
            -> String  -- ^ Имя типа.
            -> Type
typeVarChar n s = typeString
    #strlen n
    #name s
    #prefer PreferVarChar

typeText :: Integer -- ^ Max длина в символах.
         -> String  -- ^ Имя типа.
         -> Type
typeText n s = typeString
    #strlen n
    #name s
    #prefer PreferText


-- Проверка имени типа для целевого языка.
checkCastTo :: Type -> Errors
checkCastTo it = case type'castTo it of
    Just s -> if goodIdWithDots s
        then []
        else ["Bad type name to cast '" ++ s ++ "'."]
    _ -> []

-- Проверка поля type'minValue.
checkMinValue :: Type -> Errors
checkMinValue it = case (type'kind it, type'minValue it) of
    (IntKind, Nothing) -> ["minValue not defined for IntKind."]
    (IntKind, Just v) -> if v > 0
        then ["minValue > 0 for IntKind."]
        else []
    (_, Nothing) -> []
    (kind, Just _) -> ["minValue defined for " ++ show kind ++ "."]

-- Проверка поля type'maxValue.
checkMaxValue :: Type -> Errors
checkMaxValue it = case (type'kind it, type'maxValue it) of
    (IntKind, Nothing) -> ["maxValue not defined for IntKind."]
    (IntKind, Just v) -> if v < 0
        then ["maxValue < 0 for IntKind."]
        else []
    (BitsKind, Nothing) -> ["maxValue not defined for BitsKind."]
    (BitsKind, Just v) -> if v < 0
        then ["maxValue < 0 for BitsKind."]
        else []
    (_, Nothing) -> []
    (kind, Just _) -> ["maxValue defined for " ++ show kind ++ "."]


-- Проверка поля type'digits.
checkDigits :: Type -> Errors
checkDigits it = case (type'kind it, type'digits it) of
    (DecimalKind, Nothing) -> ["digits not defined for DecimalKind."]
    (DecimalKind, Just v) -> if v <= 0
        then ["digits <= 0 for DecimalKind."]
        else []
    (_, Nothing) -> []
    (kind, Just _) -> ["digits defined for " ++ show kind ++ "."]

-- Проверка поля type'precision.
checkPrecision :: Type -> Errors
checkPrecision it = case (type'kind it, type'precision it) of
    (DecimalKind, Nothing) -> ["precision not defined for DecimalKind."]
    (DecimalKind, Just v) -> if v <= 0
        then ["precision <= 0 for DecimalKind."]
        else []
    (_, Nothing) -> []
    (kind, Just _) -> ["precision defined for " ++ show kind ++ "."]

-- Проверка соотношения полей type'digits и type'precision.
checkDP :: Type -> Errors
checkDP it = case (type'kind it, type'digits it, type'precision it) of
    (DecimalKind, Just d, Just p) -> if p > d
        then ["precision > digits for DecimalKind."]
        else []
    (_, _, _) -> []

-- Проверка поля type'mantissa.
checkMantissa :: Type -> Errors
checkMantissa it = case (type'kind it, type'mantissa it) of
    (FloatKind, Nothing) -> ["mantissa not defined for FloatKind."]
    (FloatKind, Just v) -> if v < 0
        then ["mantissa < 0 for FloatKind."]
        else []
    (_, Nothing) -> []
    (kind, Just _) -> ["mantissa defined for " ++ show kind ++ "."]

-- Проверка поля type'expBits.
checkExpBits :: Type -> Errors
checkExpBits it = case (type'kind it, type'expBits it) of
    (FloatKind, Nothing) -> ["expBits not defined for FloatKind."]
    (FloatKind, Just v) -> if v < 0
        then ["expBits < 0 for FloatKind."]
        else []
    (_, Nothing) -> []
    (kind, Just _) -> ["expBits defined for " ++ show kind ++ "."]

-- Проверка поля type'encoding.
checkEncoding :: Type -> Errors
checkEncoding it = case (type'kind it, type'encoding it) of
    (StringKind, Nothing) -> ["encoding not defined for StringKind."]
    (StringKind, Just _) -> []
    (kind, Just _) -> ["encoding defined for " ++ show kind ++ "."]
    (_, _) -> []
   
-- Проверка поля type'prefer.
checkPrefer :: Type -> Errors
checkPrefer it = case (type'kind it, type'prefer it) of
    (StringKind, Nothing) -> ["prefer not defined for StringKind."]
    (StringKind, Just _) -> []
    (kind, Just _) -> ["prefer defined for " ++ show kind ++ "."]
    (_, _) -> []

-- Проверка поля type'strlen.
checkStrlen :: Type -> Errors
checkStrlen it = case (type'kind it, type'strlen it) of
    (StringKind, Nothing) -> ["strlen not defined for StringKind."]
    (StringKind, Just v) -> if v <= 0
        then ["strlen <= 0 for StringKind."]
        else []
    (_, Nothing) -> []
    (kind, Just _) -> ["strlen defined for " ++ show kind ++ "."]

-- Проверка типа данных, возвращает список ошибок.
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


-- Имя типа данных.
instance HasName Type where
    getName = type'name
    name v it = it{ type'name = v }
    getTitle it = "type '" ++ type'name it ++ "'"


-- Комментарий к типу данных.
instance HasComment Type where
    getComment = type'comment
    comment v it = it{ type'comment = v }


-- Допустимы ли NULL-значения для типа данных.
getNullable :: Type -> Bool
getNullable = type'nullable

nullable :: Bool -> Type -> Type
nullable v it = it{ type'nullable = v }

maybeNull :: Type -> Type
maybeNull a = nullable True a

notNull :: Type -> Type
notNull a = nullable False a


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
    Nothing -> error (getTitle it ++ " type supports minValue property.")

getMaxValue :: Type -> Integer
getMaxValue it = case type'maxValue it of
    Just v -> v
    Nothing -> error (getTitle it ++ " not supports maxValue property.")

getDigits :: Type -> Int
getDigits it = case type'digits it of
    Just v -> v
    Nothing -> error (getTitle it ++ " not supports digits property.")

getPrecision :: Type -> Int
getPrecision it = case type'precision it of
    Just v -> v
    Nothing -> error (getTitle it ++ " not supports precision property.")

getMantissa :: Type -> Int
getMantissa it = case type'mantissa it of
    Just v -> v
    Nothing -> error (getTitle it ++ " not supports mantissa property.")

getExpBits :: Type -> Int
getExpBits it = case type'expBits it of
    Just v -> v
    Nothing -> error (getTitle it ++ " not supports expBits property.")

getEncoding :: Type -> Encoding
getEncoding it = case type'encoding it of
    Just v -> v
    Nothing -> error (getTitle it ++ " not supports encoding property.")

getPrefer :: Type -> Prefer
getPrefer it = case type'prefer it of
    Just v -> v
    Nothing -> error (getTitle it ++ " not supports prefer property.")

getStrlen :: Type -> Integer
getStrlen it = case type'strlen it of
    Just v -> v
    Nothing -> error (getTitle it ++ " not supports strlen property.")


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
    _ -> error (getTitle it ++ " not supports bits property.")

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

-- Можно ли хранить тип it как 80-битное число с плавающей точкой.
asFloat80 :: Type -> Bool
asFloat80 it = asFloat it 64 15

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






















