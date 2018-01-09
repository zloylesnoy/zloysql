module SqlType (
    sqlTypeName
) where

import Common
import Type


useAutoIncrementInMySQL   = True
useIdentityInMicrosoftSQL = False
useSerialInPostgreSQL     = False


-- |Max размер символа в байтах в кодировке UTF-8.
--  В кодировке UNICODE UTF-8 символ занимает 1-4 байта.
--  Но если использовать только UCS-2 UTF-8, тогда символ занимает 1-3 байта.
maxUtf8Size :: Encoding -> Integer
maxUtf8Size Ascii = 1
maxUtf8Size Ucs2  = 3

-- |Генерировать название строкового типа на указанном языке.
--  Последний параметр - это длина строки в символах.
stringTypeName :: Language -> Prefer -> Encoding -> Integer -> Maybe String

-- Реализация для MicrosoftSQL.
-- Типы данных TEXT и NTEXT устарели и мы не будем их использовать.

-- |Типы VARCHAR(MAX) и NVARCHAR(MAX) могут иметь длину
--  до 2^31-3 байт (~2ГБ) в Microsoft SQL Server.
msMaxStringSize = pow2x 31 - 3

-- |Типы CHAR, NCHAR, VARCHAR и NVARCHAR могут иметь длину
--  до 8000 байт в Microsoft SQL Server.
msMaxStringLength = 8000

stringTypeName MicrosoftSQL p Ascii n
    | n > msMaxStringSize   = Nothing
    | n > msMaxStringLength = Just "VARCHAR(MAX)"
    | otherwise             = case p of
        PreferChar    -> Just ("CHAR(" ++ (show n) ++ ")")
        PreferVarChar -> Just ("VARCHAR(" ++ (show n) ++ ")")
        PreferText    -> Just "VARCHAR(MAX)"

stringTypeName MicrosoftSQL p Ucs2 n
    | n * 2 > msMaxStringSize   = Nothing
    | n * 2 > msMaxStringLength = Just "NVARCHAR(MAX)"
    | otherwise                 = case p of
        PreferChar    -> Just ("NCHAR(" ++ (show n) ++ ")")
        PreferVarChar -> Just ("NVARCHAR(" ++ (show n) ++ ")")
        PreferText    -> Just "NVARCHAR(MAX)"

-- Реализация для MySQL.

stringTypeName MySQL p enc n
    | canUseChar      = Just(   "CHAR(" ++ (show n) ++ ")" ++ suffix)
    | canUseVarChar   = Just("VARCHAR(" ++ (show n) ++ ")" ++ suffix)
    | size < pow2x 8  = Just(  "TINYTEXT" ++ suffix)
    | size < pow2x 16 = Just(      "TEXT" ++ suffix)
    | size < pow2x 24 = Just("MEDIUMTEXT" ++ suffix)
    | size < pow2x 32 = Just(  "LONGTEXT" ++ suffix)
    | otherwise       = Nothing
  where
    size = n * maxUtf8Size enc -- max размер строки в байтах

    suffix = case enc of
        Ascii -> " CHARACTER SET ascii COLLATE ascii_bin"
        Ucs2  -> " CHARACTER SET utf8 COLLATE utf8_unicode_ci"

    -- Размер CHAR должен быть не более 255 символов.
    myCharMaxLength = 255
    canUseChar = (p == PreferChar) && (n <= myCharMaxLength)

    -- В старых версиях размер VARCHAR должен быть не более 255 символов.
    myOldVarCharMaxLength = 255
    c1 = (p /= PreferText) && (n <= myOldVarCharMaxLength)
    -- Сейчас размер VARCHAR ограничен тем условием, что запись в таблице
    -- должна занимать не более 65,535 байт. (Зависит от движка таблиц.)
    myVarCharMaxSize = 65535
    c2 = (p == PreferVarChar) && (n * maxUtf8Size enc <= myVarCharMaxSize)
    canUseVarChar = c1 || c2

-- Реализация для PostgreSQL.
-- В Postgres SQL эффективнее использовать VARCHAR вместо CHAR.
-- Размер строки ограничен одним гигабайтом.
-- TODO: " WITH ENCODING='ASCII'", " WITH ENCODING='UTF8'" - не разобрался.

stringTypeName PostgreSQL p enc n =
    if n * maxUtf8Size enc < pgMaxStringSize
        then Just("VARCHAR(" ++ (show n) ++ ")")
        else Nothing
  where
    pgMaxStringSize = pow2x 30


sqlTypeName :: Language -> Type -> Bool -> Maybe String

-- Реализация для MicrosoftSQL.

sqlTypeName MicrosoftSQL it identity
    | identity && getNullable it = Nothing

    | asUnsignedInteger it  8 = Just $  "TINYINT" ++ suffix
    | asSignedInteger   it 16 = Just $ "SMALLINT" ++ suffix
    | asSignedInteger   it 32 = Just $      "INT" ++ suffix
    | asSignedInteger   it 64 = Just $   "BIGINT" ++ suffix

    -- Microsoft SQL Server поддерживает тип DECIMAL, до 38 десятичных цифр.
    -- DECIMAL(9)  -  4 байт + 1 байт для знака (для целых лучше BIGINT)
    -- DECIMAL(19) -  8 байт + 1 байт для знака (это на один бит больше, чем BIGINT)
    -- DECIMAL(28) - 12 байт + 1 байт для знака
    -- DECIMAL(38) - 16 байт + 1 байт для знака
    | asDecimal it  9 0 = Just $ "DECIMAL(9)"  ++ suffix -- никогда не сработает
    | asDecimal it 19 0 = Just $ "DECIMAL(19)" ++ suffix
    | asDecimal it 28 0 = Just $ "DECIMAL(28)" ++ suffix
    | asDecimal it 38 0 = Just $ "DECIMAL(38)" ++ suffix

    -- MS SQL поддерживает IDENTITY только для целых и DECIMAL(*, 0).
    | identity = Nothing

    | getKind it == StringKind =
        case stringTypeName MicrosoftSQL (getPrefer it) (getEncoding it) (getStrlen it) of
            Just stn -> Just $ stn ++ suffix
            Nothing  -> Nothing

    | asFloat32 it = Just $ "FLOAT(24)" ++ suffix
    | asFloat64 it = Just $ "FLOAT(53)" ++ suffix

    | getKind it == DecimalKind = itIsDecimal

    | otherwise = Nothing
  where
    nul = if getNullable it then " NULL" else " NOT NULL"
    suffix = if identity && useIdentityInMicrosoftSQL then nul ++ " IDENTITY" else nul
    prec = getPrecision it
    justDec k = Just ("DECIMAL(" ++ show k  ++ ", " ++ show prec ++ ")" ++ suffix)
    itIsDecimal
        | asDecimal it  9 prec = justDec  9
        | asDecimal it 19 prec = justDec 19
        | asDecimal it 28 prec = justDec 28
        | asDecimal it 38 prec = justDec 38
        | otherwise            = Nothing


-- Реализация для MySQL.

sqlTypeName MySQL it identity
    | identity && getNullable it = Nothing

    | asUnsignedInteger  it  8 = Just $  "UNSIGNED TINYINT" ++ suffix
    | asSignedInteger    it  8 = Just $           "TINYINT" ++ suffix
    | asSignedInteger    it 16 = Just $          "SMALLINT" ++ suffix
    | asUnsignedInteger  it 16 = Just $ "UNSIGNED SMALLINT" ++ suffix
    | asSignedInteger    it 32 = Just $               "INT" ++ suffix
    | asUnsignedInteger  it 32 = Just $      "UNSIGNED INT" ++ suffix
    | asSignedInteger    it 64 = Just $            "BIGINT" ++ suffix
    | asUnsignedInteger  it 64 = Just $   "UNSIGNED BIGINT" ++ suffix

    | asFloat32 it = Just $ "FLOAT"  ++ suffix
    | asFloat64 it = Just $ "DOUBLE" ++ suffix

    -- MySQL поддерживает AUTO_INCREMENT для целых и чисел с плавающей точкой.
    | identity = Nothing

    | getKind it == StringKind =
        case stringTypeName MySQL (getPrefer it) (getEncoding it) (getStrlen it) of
            Just stn -> Just $ stn ++ suffix
            Nothing  -> Nothing

    | otherwise = decTypeName it suffix myMaxDecimalDigits
  where
    nul = if getNullable it then " NULL" else " NOT NULL"
    suffix = if identity && useAutoIncrementInMySQL then nul ++ " AUTO_INCREMENT" else nul

    -- MySQL поддерживает тип DECIMAL, до 65 десятичных цифр.
    -- Каждые 9 десятичных цифр хранятся в 4-байтном целом.
    myMaxDecimalDigits = 65


-- Реализация для PostgreSQL.

sqlTypeName PostgreSQL it identity
    | identity && useSerialInPostgreSQL = itIsIdentity

    | asSignedInteger it 16 = Just $ "SMALLINT" ++ suffix
    | asSignedInteger it 32 = Just $      "INT" ++ suffix
    | asSignedInteger it 64 = Just $   "BIGINT" ++ suffix

    | asFloat32 it = Just $ "REAL" ++ suffix
    | asFloat64 it = Just $ "DOUBLE PRECISION" ++ suffix

    | getKind it == StringKind =
        case stringTypeName PostgreSQL (getPrefer it) (getEncoding it) (getStrlen it) of
            Just stn -> Just $ stn ++ suffix
            Nothing  -> Nothing

    | otherwise = decTypeName it suffix pgMaxDecimalDigits
  where
    suffix = if getNullable it then " NULL" else " NOT NULL"

    -- Postgres SQL поддерживает тип DECIMAL, до 1000 десятичных цифр.
    -- Каждые 4 цифры занимают 2 байта, видимо используется BCD.
    pgMaxDecimalDigits = 1000

    itIsIdentity
        | getNullable it       = Nothing
        | asSignedInteger it 16 = Just "SMALLSERIAL"
        | asSignedInteger it 32 = Just      "SERIAL"
        | asSignedInteger it 64 = Just   "BIGSERIAL"
        | otherwise             = Nothing


-- Третий параметр - max число цифр, которое поддерживает DECIMAL.
decTypeName :: Type -> String -> Int -> Maybe String
decTypeName it suffix maxDigits
    | (decint > 0) && (decint <= maxDigits) =
        Just $ "DECIMAL("  ++ show decint ++ ")" ++ suffix

    | getKind it == DecimalKind = let
        prec = getPrecision it
        dig  = max (getDigits it) prec
      in
        if dig <= maxDigits
            then Just $ "DECIMAL("  ++ show dig ++ ", " ++ show prec ++ ")" ++ suffix
            else Nothing

    | otherwise = Nothing
  where
    decint = asDecimalInt it
























