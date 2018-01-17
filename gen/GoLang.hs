module GoLang (
    module Sch,

    golang
) where

import Sch

{-
Code generator for Go language using
https://github.com/go-sql-driver/mysql
-}

data GoTypeKind
    = GoStd -- predefined Go type or type defined in common.go file
    | GoBig -- need math/big
    | GoDec -- need github.com/shopspring/decimal
    | GoErr -- failed
    deriving (Eq)

-- Returns Go type kind and Go type name.
goTypeKindName :: Type -> (GoTypeKind, String)
goTypeKindName it
    | asSignedInteger   it  8 = std "MaybeInt8"   "int8"
    | asUnsignedInteger it  8 = std "MaybeUInt8"  "uint8"
    | asSignedInteger   it 16 = std "MaybeInt16"  "int16"
    | asUnsignedInteger it 16 = std "MaybeUInt16" "uint16"
    | asSignedInteger   it 32 = std "MaybeInt32"  "int32"
    | asUnsignedInteger it 32 = std "MaybeUInt32" "uint32"
    | asSignedInteger   it 64 = std "MaybeInt64"  "int64"
    | asUnsignedInteger it 64 = std "MaybeUInt64" "uint64"

    | getKind it == IntKind = (GoBig, "*big.Int") -- SQL NULL is Go nil

    | asFloat32 it = std "MaybeFloat32" "float32"
    | asFloat64 it = std "MaybeFloat64" "float64"

    | getKind it == StringKind = std "MaybeString" "string"

    | getKind it == DecimalKind = if getPrecision it == 0
        then (GoBig, "*big.Int") -- SQL NULL is Go nil
        else (GoDec, if nul then "MaybeDecimal" else "*decimal.Decimal")

    | otherwise = (GoErr, "// ERROR: Can not generate Go type.")
  where
    nul = getNullable it
    std a b = (GoStd, if nul then a else b)

-- Returns Go type kind and Go type definition.
goTypeKindDef :: Type -> (GoTypeKind, [String])
goTypeKindDef t = (kind, def)
  where
    (kind, name) = goTypeKindName t
    def = cppComment t ++ ["type T_" ++ getName t ++ " = " ++ name, ""]


goField :: Field -> [String]
goField fld = case length rem of
    0 -> [nameType]
    1 -> [nameType ++ " " ++ head rem]
    _ -> rem ++ [nameType]
  where
    rem = cppComment fld
    nameType = "\tM_" ++ getName fld ++ " T_" ++ getName (getType fld)

goRecord :: Record -> [String]
goRecord r = cppComment r
    ++ ["type S_" ++ getName r ++ " struct {"]
    ++ concat (map goField $ getFields r)
    ++ ["}", ""]

golang :: Scheme -> String
golang sch = unlines $ [
    "package zloysql",
    "",
    "import (",
    "\t\"database/sql\""]
    ++ importBig
    ++ importDec
    ++ [")", ""]
    ++ concat typeDefs
    ++ concat recordDefs
  where
    typeKindDefs :: [(GoTypeKind, [String])]
    typeKindDefs = map goTypeKindDef (getTypes sch)

    typeKinds :: [GoTypeKind]
    typeKinds = map fst typeKindDefs

    typeDefs :: [[String]]
    typeDefs = map snd typeKindDefs

    importBig = if any (== GoBig) typeKinds then ["\t\"math/big\""] else []
    importDec = if any (== GoDec) typeKinds then ["", "\t\"github.com/shopspring/decimal\""] else []

    recordDefs :: [[String]]
    recordDefs = map goRecord $ getRecords sch



































