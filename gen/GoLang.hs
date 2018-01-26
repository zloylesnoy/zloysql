module GoLang (
    module Sch,

    golang
) where

import Data.List (isPrefixOf)
import Data.String.Utils (join)

import Sch
import Sql


{-
Code generator for Go language using
https://github.com/go-sql-driver/mysql
-}

tab :: Char
tab = '\t'

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
        else (GoDec, if nul then "MaybeDecimal" else "decimal.Decimal")

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
    nameType = tab:"M_" ++ getName fld ++ " T_" ++ getName (getType fld)

goRecord :: Record -> [String]
goRecord r = cppComment r
    ++ ["type S_" ++ getName r ++ " struct {"]
    ++ concat (map goField $ getFields r)
    ++ ["}", ""]

golang :: DialectSQL -> Scheme -> String
golang lang sch = unlines $ [
    "package zloysql", "",

    "import (",
    tab:"\"database/sql\""]
    ++ importBig
    ++ importDec
    ++ [")", ""]

    ++ cppComment sch
    ++ ["type DB_" ++ getName sch ++ " struct {", tab:"P *sql.DB", "}", ""]

    ++ concat typeDefs
    ++ concat recordDefs
    ++ concat deleteFuncs
    ++ concat updateFuncs
    ++ concat selectFuncs
  where
    typeKindDefs :: [(GoTypeKind, [String])]
    typeKindDefs = map goTypeKindDef (getTypes sch)

    typeKinds :: [GoTypeKind]
    typeKinds = map fst typeKindDefs

    typeDefs :: [[String]]
    typeDefs = map snd typeKindDefs

    importBig = if any (== GoBig) typeKinds then [tab:"\"math/big\""] else []
    importDec = if any (== GoDec) typeKinds then ["", tab:"\"github.com/shopspring/decimal\""] else []

    recordDefs :: [[String]]
    recordDefs = map goRecord $ filter (not.null.getFields) (getRecords sch)

    deleteFuncs :: [[String]]
    deleteFuncs = map (goExec lang) (getDeletes sch)

    updateFuncs :: [[String]]
    updateFuncs = map (goExec lang) (getUpdates sch)

    selectFuncs :: [[String]]
    selectFuncs = map (goQuery lang) (getSelects sch)


-- Create local varibale for SQL query result.
goOutputVar :: DialectSQL -> (Int, Field) -> String
goOutputVar lang (idx, fld) = tab:"var r" ++ show idx ++ " " ++ driverType
  where
    driverType = goSqlDriverType lang $ getType fld

goSqlDriverType :: DialectSQL -> Type -> String
goSqlDriverType lang t
    | getNullable t = goNullableSqlDriverType lang t
    -- Type is not nullable.
    | (asUnsignedInteger t  8) && (lang /= PostgreSQL) = "uint8"
    | (asSignedInteger   t  8) && (lang /= PostgreSQL) = "int8"
    | (asSignedInteger   t 16) = "int16"
    | (asUnsignedInteger t 16) && (lang == MySQL) = "uint16"
    | (asSignedInteger   t 32) = "int32"
    | (asUnsignedInteger t 32) && (lang == MySQL) = "uint32"
    | (asSignedInteger   t 64) = "int64"
    | (asUnsignedInteger t 64) = "uint64"
    -- Type is not binary integer.
    | asFloat32 t = "float32"
    | asFloat64 t = "float64"
    | getKind t == StringKind  = "string"
    | getKind t == DecimalKind = "[]byte"
    -- Type is not supported.
    | otherwise = error $ "Go type not supported " ++ getName t

goNullableSqlDriverType :: DialectSQL -> Type -> String
goNullableSqlDriverType lang t
    | asSignedInteger   t 64   = "sql.NullInt64"
    | asUnsignedInteger t 64   = "sql.NullInt64"
    | asFloat64 t              = "sql.NullFloat64"
    | getKind t == StringKind  = "sql.NullString"
    | getKind t == DecimalKind = "[]byte" -- SQL 'NULL' is 'nil' in Go language.
    | otherwise                = error $ "Go nullable type not supported " ++ getName t

goExec :: (HasName q, HasSql q, HasParams q) => DialectSQL -> q -> [String]
goExec lang sql = [
    "func (it DB_sp) Q_" ++ getName sql ++ "(" ++ funcParams ++ ") (int64, error) {",
    tab:"if it.P == nil {",
    tab:tab:"return 0, ErrorSqlDbIsNil",
    tab:"}",
    "",
    tab:"const query string = " ++ queryString]
    ++ qVars ++
    ["",
    tab:"var res sql.Result",
    tab:"var err error",
    tab:"res, err = it.P.Exec(query" ++ concat qParams ++ ")",
    "",
    tab:"var affected int64 = 0",
    tab:"if res != nil && err == nil {",
    tab:tab:"affected, err = res.RowsAffected()",
    tab:"}",
    "",
    tab:"return affected, err",
    "}",
    ""]
  where
    swp :: SqlWithParams
    swp = getSql lang sql

    queryString :: String
    queryString = "\"" ++ goEscaped (getQueryString swp) ++ "\""

    paramsRec :: Record
    paramsRec = getParams sql

    funcParams :: String
    funcParams = if null $ getFields paramsRec
        then ""
        else "a S_" ++ getName paramsRec

    paramFields :: [Field]
    paramFields = selectByNames (getFields paramsRec) (getParamNames swp)

    qNumbered :: [(Int, Field)]
    qNumbered = zip [0..] paramFields

    qParams :: [String]
    qParams = map (\p -> ", q" ++ show (fst p)) qNumbered

    qVars :: [String]
    qVars = map (goInputVar lang) qNumbered


goQuery :: (HasName q, HasSql q, HasParams q, HasResult q) => DialectSQL -> q -> [String]
goQuery lang sql = [
    "func (it DB_sp) Q_" ++ getName sql ++ "(" ++ funcParams ++ ") ([]S_" ++ getName resultRec ++ ", error) {",
    tab:"if it.P == nil {",
    tab:tab:"return nil, ErrorSqlDbIsNil",
    tab:"}",
    "",
    tab:"const query string = " ++ queryString]
    ++ qVars ++ "":rVars ++
    ["",
    tab:"var rows *sql.Rows",
    tab:"var err error",
    tab:"rows, err = it.P.Query(query" ++ concat qParams ++ ")",
    tab:"if err != nil {",
    tab:tab:"return nil, err",
    tab:"}",
    tab:"defer rows.Close()",
    "",
    tab:"var result []S_" ++ getName resultRec,
    tab:"var item S_" ++ getName resultRec,
    tab:"for rows.Next() {",
    tab:tab:"err = rows.Scan(" ++ join ", " rParams ++ ")",
    tab:tab:"if err != nil {",
    tab:tab:tab:"return nil, err",
    tab:tab:"}",
    ""]
    ++ rSetItem ++
    ["",
    tab:tab:"result = append(result, item)",
    tab:"}",
    tab:"err = rows.Err()",
    tab:"if err != nil {",
    tab:tab:"return nil, err",
    tab:"}",
    "",
    tab:"return result, nil",
    "}",
    ""]
  where
    swp :: SqlWithParams
    swp = getSql lang sql

    queryString :: String
    queryString = "\"" ++ goEscaped (getQueryString swp) ++ "\""

    paramsRec :: Record
    paramsRec = getParams sql

    resultRec :: Record
    resultRec = getResult sql

    funcParams :: String
    funcParams = if null $ getFields paramsRec
        then ""
        else "a S_" ++ getName paramsRec

    paramFields :: [Field]
    paramFields = selectByNames (getFields paramsRec) (getParamNames swp)

    qNumbered :: [(Int, Field)]
    qNumbered = zip [0..] paramFields

    rNumbered :: [(Int, Field)]
    rNumbered = zip [0..] $ getFields resultRec

    qParams :: [String]
    qParams = map (\p -> ", q" ++ show (fst p)) qNumbered

    rParams :: [String]
    rParams = map (\p -> "&r" ++ show (fst p)) rNumbered

    qVars :: [String]
    qVars = map (goInputVar lang) qNumbered

    rVars :: [String]
    rVars = map (goOutputVar lang) rNumbered

    rSetItem :: [String]
    rSetItem = map (goSetItem lang) rNumbered

-- Create local varibale for SQL query input paramter.
goInputVar :: DialectSQL -> (Int, Field) -> String
goInputVar lang (idx, fld) = tab:"var q" ++ show idx ++ " " ++ driverType ++ " = " ++ value ++ " // " ++ baseType
  where
    nm = getName fld
    tp = getType fld
    driverType = goSqlDriverType lang tp
    baseType = snd $ goTypeKindName tp

    value :: String
    value | baseType == driverType         = "a.M_" ++ nm
          | "sql." `isPrefixOf` driverType = "a.M_" ++ nm ++ ".ToSqlDriver()"
          | baseType == "*big.Int"         = "BigIntToBytes(a.M_" ++ nm ++ ")"
          | baseType == "decimal.Decimal"  = "DecimalToBytes(a.M_" ++ nm ++ ")"
          | driverType == "[]byte"         = "a.M_" ++ nm ++ ".ToSqlDriver()"
          | otherwise                      = driverType ++ "(a.M_" ++ nm ++ ")"

goSetItem :: DialectSQL -> (Int, Field) -> String
goSetItem lang (idx, fld) = tab:tab:"item.M_" ++ getName fld ++ value ++ "// " ++ baseType ++ " " ++ driverType
  where
    n = show idx
    tp = getType fld
    driverType = goSqlDriverType lang tp
    baseType = snd $ goTypeKindName tp

    value :: String
    value | baseType == driverType        = " = r" ++ n
          | "Maybe" `isPrefixOf` baseType = ".FromSqlDriver(r" ++ n ++ ")"
          | baseType == "*big.Int"        = " = BigIntFromBytes(r" ++ n ++ ")"
          | driverType == "[]byte"        = " = DecimalFromBytes(r" ++ n ++ ")"
          | otherwise                     = " = " ++ baseType ++ "(r" ++ n ++ ")"





























