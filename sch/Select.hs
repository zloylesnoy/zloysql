module Select (
    HasTables, innerTables, innerRecords,

    Expression(..), simplify,
    using, not_, inv, neg, exists, not_exists,
    (.+.), (.-.), (.*.), (./.), (.%.),
    (.&&.), (.||.), (.&.), (.|.), xor,
    (.==.), (.!=.), (.<.), (.>.), (.<=.), (.>=.),
    (.==.?), (.!=.?), (.<.?), (.>.?), (.<=.?), (.>=.?),
    (.==.*), (.!=.*), (.<.*), (.>.*), (.<=.*), (.>=.*),

    From (..), Select, select,
    HasParams, params, getParams, nameParams,
    getFrom,
    HasWhere, where_, getWhere, notWhere, andWhere, orWhere,
    getResult,
    bind, getBinds, getBind,
    distinct, getDistinct,
    groupBy, getGroupBy,
    having, getHaving,
    orderBy, getOrderBy,
    limit, limit_, noLimit, getLimit,
    offset, offset_, getOffset,

    readOne, readAll, whereGiven, returnAs, returnOnly
) where

import Data.String.Utils (join)
import qualified Data.Map.Strict as Map

import Common
import Escape
import Type
import Field
import Record
import Table


-- |Сложное определение, которое содержит в себе определения таблиц и записей.
class HasTables it where
    -- |Возвращает список таблиц, определённых внутри it.
    --  Таблицы в списке могут повторяться.
    innerTables :: it -> [Table]
    -- |Возвращает список записей, определённых внутри it.
    --  Записи в списке могут повторяться. Записи, которые определяют
    --  структуру таблиц из innerTables могут не входить в список.
    innerRecords :: it -> [Record]


{-
Выражения встречаются в SQL в разных местах:
  - SELECT WHERE Expression
  - JOIN ON Expression
  - JOIN USING Expression
  - DELETE WHERE Expression
  - UPDATE WHERE Expression
  - SELECT Name AS Expression
  - UPDATE SET Field = Expression

Внутри выражения может быть вложенный запрос SELECT.
  - SELECT ... WHERE field NOT IN (SELECT ...)  -- запрос возвращает массив значений
  - SELECT ... WHERE field = (SELECT ...) -- запрос возвращает одно значение
IN, NOT IN, <= ANY, <= ALL, EXISTS, NOT EXISTS

Поэтому Select и Expression определены взаимно рекурсивно.
  
Когда мы проверяем правильность выражения и ищем его тип,
мы не полагаемся на систему типов Haskell, а делаем всё сами.
Поэтому у нас один рекурсивный тип данных Expression для выражения.

Выражение содержит внутри определения используемых таблиц и параметров.


Коррелированные вложенные запросы
http://kuzelenkov.narod.ru/mati/book/progr/progr3.html

SELECT DISTINCT c.LastName, c.FirstName, e.BusinessEntityID 
FROM Person.Person AS c JOIN HumanResources.Employee AS e
ON e.BusinessEntityID = c.BusinessEntityID 
WHERE 5000.00 IN
    (SELECT Bonus
    FROM Sales.SalesPerson sp
    WHERE e.BusinessEntityID = sp.BusinessEntityID)

SELECT DISTINCT pv1.ProductID, pv1.BusinessEntityID
FROM Purchasing.ProductVendor pv1
WHERE ProductID IN
    (SELECT pv2.ProductID
     FROM Purchasing.ProductVendor pv2
     WHERE pv1.BusinessEntityID <> pv2.BusinessEntityID)
ORDER  BY pv1.BusinessEntityID


-}

-- Ещё надо сделать:
-- boolean_primary comparison_operator {ALL | ANY} (subquery)
-- bit_expr [NOT] IN (subquery)


data Expression

    -- |Значение NULL.
    = ExprNull

    -- |Логическая константа.
    | ExprFalse
    | ExprTrue

    -- |Целочисленная константа.
    | ExprIntegerConst Integer

    -- |Константа с плавающей точкой.
    | ExprDoubleConst Double

    -- |Десятичная константа (мантисса, число цифр после запятой)
    | ExprDecimalConst Integer Int

    -- |Строковая константа.
    | ExprStringConst String

    -- |Унарный оператор SQL, имя оператора хранится как String.
    --  "NOT" "~" "-"
    --  "EXISTS", "NOT EXISTS"
    | ExprUnary String Expression

    -- |Бинарный оператор SQL, имя оператора хранится как String.
    --  "+" "-" "*" "/" "%"
    --  "AND" "OR" "&" "|" "^"
    --  "=" "!=" "<" ">" "<=" ">="
    --  "= ANY" "!= ANY" "< ANY" "> ANY" "<= ANY" ">= ANY"
    --  "= ALL" "!= ALL" "< ALL" "> ALL" "<= ALL" ">= ALL"
    --  "IN" "NOT IN"
    | ExprBinary Expression String Expression

    -- |Параметр запроса (имя параметра, тип параметра).
    | ExprParam String Type

    -- |Значение поля таблицы (таблица, имя поля).
    | ExprField Table String

    -- |Вложенный запрос SELECT.
    | ExprSelect Select

    -- |Значение из внешнего запроса (псевдоним, имя поля, тип поля).
    | ExprOuter String String Type

    -- |Используется в секции FROM ... JOIN ... USING ...
    --  Содержит список имён полей, которые должны быть равны.
    | ExprJoinUsing [String]

    -- |Используется в UPDATE table SET column = DEFAULT
    | ExprDefault

    -- |Используется в секции SET оператора UPDATE
    --  Первый параметр должен быть ExprField.
    | ExprSet Expression Expression

    deriving (Eq, Show)

instance ToText Expression where
    toText ExprNull = "NULL"
    toText ExprFalse = "FALSE"
    toText ExprTrue = "TRUE"
    toText (ExprIntegerConst x) = show x
    toText (ExprDoubleConst x) = show x
    toText (ExprDecimalConst digs prec) = showDecimal digs prec
    toText (ExprStringConst x) = show x
    toText (ExprUnary oper e1) = "(" ++ oper ++ toText e1 ++ ")"
    toText (ExprBinary e1 oper e2) = "(" ++ toText e1 ++ " " ++ oper ++ " " ++ toText e2 ++ ")"
    toText (ExprParam nm tp) = "{$" ++ nm ++ "::" ++ getName tp ++ "}"
    toText (ExprField tab nm) = getName tab ++ "." ++ nm
    toText (ExprSelect sel) = "(\n" ++ toText sel ++ "\n)"
    toText (ExprOuter alias nm tp) = "{^" ++ alias ++ "." ++ nm ++ "::" ++ getName tp ++ "}"
    toText (ExprJoinUsing xs) = "{USING " ++ show xs ++ "}"
    toText ExprDefault = "DEFAULT"
    toText (ExprSet var val) = toText var ++ " = " ++ toText val

instance HasTables Expression where
    innerTables expr = case expr of
        ExprUnary _ e1     -> innerTables e1
        ExprBinary e1 _ e2 -> innerTables e1 ++ innerTables e2
        ExprField tab _    -> [tab]
        ExprSelect sel     -> innerTables sel
        ExprSet e1 e2      -> innerTables e1 ++ innerTables e2
        _                  -> []
    innerRecords expr = case expr of
        ExprUnary _ e1     -> innerRecords e1
        ExprBinary e1 _ e2 -> innerRecords e1 ++ innerRecords e2
        ExprSelect sel     -> innerRecords sel
        ExprSet e1 e2      -> innerRecords e1 ++ innerRecords e2
        _                  -> []


using :: [String] -> Expression
using ss = ExprJoinUsing ss

-- |Логическое НЕ.
not_ :: Expression -> Expression
not_ e1 = ExprUnary "NOT" e1

-- |Побитовое НЕ.
inv :: Expression -> Expression
inv e1 = ExprUnary "~" e1

-- |Смена знака.
neg :: Expression -> Expression
neg e1 = ExprUnary "-" e1

exists :: Expression -> Expression
exists e1 = ExprUnary "EXISTS" e1

not_exists :: Expression -> Expression
not_exists e1 = ExprUnary "NOT EXISTS" e1

-- |Сложение чисел, конкатенация строк.
infixl 6 .+.
(.+.) :: Expression -> Expression -> Expression
e1 .+. e2 = ExprBinary e1 "+" e2

-- |Вычитание.
infixl 6 .-.
(.-.) :: Expression -> Expression -> Expression
e1 .-. e2 = ExprBinary e1 "-" e2

-- |Умножение.
infixl 7 .*.
(.*.) :: Expression -> Expression -> Expression
e1 .*. e2 = ExprBinary e1 "*" e2

-- |Деление.
infixl 7 ./.
(./.) :: Expression -> Expression -> Expression
e1 ./. e2 = ExprBinary e1 "/" e2

-- |Остаток от деления.
infixl 7 .%.
(.%.) :: Expression -> Expression -> Expression
e1 .%. e2 = ExprBinary e1 "%" e2

-- |Логическое И.
infixr 3 .&&.
(.&&.) :: Expression -> Expression -> Expression
e1 .&&. e2 = ExprBinary e1 "AND" e2

-- |Логическое ИЛИ.
infixr 2 .||.
(.||.) :: Expression -> Expression -> Expression
e1 .||. e2 = ExprBinary e1 "OR" e2

-- |Побитовое И.
infixr 7 .&.
(.&.) :: Expression -> Expression -> Expression
e1 .&. e2 = ExprBinary e1 "&" e2

-- |Побитовое ИЛИ.
infixr 5 .|.
(.|.) :: Expression -> Expression -> Expression
e1 .|. e2 = ExprBinary e1 "|" e2

-- |Побитовое XOR.
infixr 6 `xor`
xor :: Expression -> Expression -> Expression
xor e1 e2 = ExprBinary e1 "^" e2


-- |Равно.
infixl 4 .==.
(.==.) :: Expression -> Expression -> Expression
e1 .==. e2 = ExprBinary e1 "=" e2

-- |Не равно.
infixl 4 .!=.
(.!=.) :: Expression -> Expression -> Expression
e1 .!=. e2 = ExprBinary e1 "!=" e2

-- |Меньше.
infixl 4 .<.
(.<.) :: Expression -> Expression -> Expression
e1 .<. e2 = ExprBinary e1 "<" e2

-- |Больше.
infixl 4 .>.
(.>.) :: Expression -> Expression -> Expression
e1 .>. e2 = ExprBinary e1 ">" e2

-- |Меньше или равно.
infixl 4 .<=.
(.<=.) :: Expression -> Expression -> Expression
e1 .<=. e2 = ExprBinary e1 "<=" e2

-- |Больше или равно.
infixl 4 .>=.
(.>=.) :: Expression -> Expression -> Expression
e1 .>=. e2 = ExprBinary e1 ">=" e2


-- |Равно одному из.
infixl 4 .==.?
(.==.?) :: Expression -> Expression -> Expression
e1 .==.? e2 = ExprBinary e1 "= ANY" e2

-- |Не равно одному из.
infixl 4 .!=.?
(.!=.?) :: Expression -> Expression -> Expression
e1 .!=.? e2 = ExprBinary e1 "!= ANY" e2

-- |Меньше одного из.
infixl 4 .<.?
(.<.?) :: Expression -> Expression -> Expression
e1 .<.? e2 = ExprBinary e1 "< ANY" e2

-- |Больше одного из.
infixl 4 .>.?
(.>.?) :: Expression -> Expression -> Expression
e1 .>.? e2 = ExprBinary e1 "> ANY" e2

-- |Меньше или равно одного из.
infixl 4 .<=.?
(.<=.?) :: Expression -> Expression -> Expression
e1 .<=.? e2 = ExprBinary e1 "<= ANY" e2

-- |Больше или равно одного из.
infixl 4 .>=.?
(.>=.?) :: Expression -> Expression -> Expression
e1 .>=.? e2 = ExprBinary e1 ">= ANY" e2


-- |Равно всем из.
infixl 4 .==.*
(.==.*) :: Expression -> Expression -> Expression
e1 .==.* e2 = ExprBinary e1 "= ALL" e2

-- |Не равно всем из.
infixl 4 .!=.*
(.!=.*) :: Expression -> Expression -> Expression
e1 .!=.* e2 = ExprBinary e1 "!= ALL" e2

-- |Меньше всех из.
infixl 4 .<.*
(.<.*) :: Expression -> Expression -> Expression
e1 .<.* e2 = ExprBinary e1 "< ALL" e2

-- |Больше всех из.
infixl 4 .>.*
(.>.*) :: Expression -> Expression -> Expression
e1 .>.* e2 = ExprBinary e1 "> ALL" e2

-- |Меньше или равно всем из.
infixl 4 .<=.*
(.<=.*) :: Expression -> Expression -> Expression
e1 .<=.* e2 = ExprBinary e1 "<= ALL" e2

-- |Больше или равно всем из.
infixl 4 .>=.*
(.>=.*) :: Expression -> Expression -> Expression
e1 .>=.* e2 = ExprBinary e1 ">= ALL" e2


-- |Присваивание в UPDATE SET ...
infixl 4 .=.
(.=.) :: Expression -> Expression -> Expression
e1 .=. e2 = ExprSet e1 e2


-- |Упрощает выражение, выполняя операторы над константными операндами.
--  TODO: Decimal
simplify :: Expression -> Expression
simplify expr = case expr of

    ExprUnary "NOT" ExprFalse -> ExprTrue
    ExprUnary "NOT" ExprTrue -> ExprFalse
    ExprUnary "NOT" (ExprUnary "EXISTS" e) -> ExprUnary "NOT EXISTS" e
    ExprUnary "NOT" (ExprUnary "NOT EXISTS" e) -> ExprUnary "EXISTS" e

    ExprUnary "-" (ExprIntegerConst c)   -> ExprIntegerConst  (0 - c)
    ExprUnary "-" (ExprDoubleConst  c)   -> ExprDoubleConst (0.0 - c)
    ExprUnary "-" (ExprDecimalConst d p) -> ExprDecimalConst  (0 - d) p

    ExprBinary (ExprIntegerConst c1) "+" (ExprIntegerConst c2) -> ExprIntegerConst (c1 + c2)
    ExprBinary (ExprDoubleConst  c1) "+" (ExprDoubleConst  c2) -> ExprDoubleConst  (c1 + c2)
    ExprBinary (ExprStringConst  c1) "+" (ExprStringConst  c2) -> ExprStringConst (c1 ++ c2)
    ExprBinary e1 "+" (ExprIntegerConst  0) -> simplify e1
    ExprBinary e1 "+" (ExprDoubleConst 0.0) -> simplify e1
    ExprBinary e1 "+" (ExprStringConst  "") -> simplify e1
    ExprBinary (ExprIntegerConst  0) "+" e2 -> simplify e2
    ExprBinary (ExprDoubleConst 0.0) "+" e2 -> simplify e2
    ExprBinary (ExprStringConst  "") "+" e2 -> simplify e2

    ExprBinary (ExprIntegerConst c1) "-" (ExprIntegerConst c2) -> ExprIntegerConst (c1 - c2)
    ExprBinary (ExprDoubleConst  c1) "-" (ExprDoubleConst  c2) -> ExprDoubleConst  (c1 - c2)
    ExprBinary e1 "-" (ExprIntegerConst  0) -> simplify e1
    ExprBinary e1 "-" (ExprDoubleConst 0.0) -> simplify e1
    ExprBinary (ExprIntegerConst  0) "-" e2 -> simplify (ExprUnary "-" $ simplify e2)
    ExprBinary (ExprDoubleConst 0.0) "-" e2 -> simplify (ExprUnary "-" $ simplify e2)

    ExprBinary (ExprIntegerConst c1) "*" (ExprIntegerConst c2) -> ExprIntegerConst (c1 * c2)
    ExprBinary (ExprDoubleConst  c1) "*" (ExprDoubleConst  c2) -> ExprDoubleConst  (c1 * c2)
    ExprBinary e1 "*" (ExprIntegerConst  0) -> ExprIntegerConst  0
    ExprBinary e1 "*" (ExprDoubleConst 0.0) -> ExprDoubleConst 0.0
    ExprBinary (ExprIntegerConst  0) "*" e2 -> ExprIntegerConst  0
    ExprBinary (ExprDoubleConst 0.0) "*" e2 -> ExprDoubleConst 0.0
    ExprBinary e1 "*" (ExprIntegerConst  1) -> simplify e1
    ExprBinary e1 "*" (ExprDoubleConst 1.0) -> simplify e1
    ExprBinary (ExprIntegerConst  1) "*" e2 -> simplify e2
    ExprBinary (ExprDoubleConst 1.0) "*" e2 -> simplify e2

    ExprBinary (ExprIntegerConst c1) "/" (ExprIntegerConst c2) -> ExprIntegerConst (div c1 c2)
    ExprBinary (ExprDoubleConst  c1) "/" (ExprDoubleConst  c2) -> ExprDoubleConst  (c1 / c2)

    ExprBinary (ExprIntegerConst c1) "%" (ExprIntegerConst c2) -> ExprIntegerConst (mod c1 c2)

    ExprBinary ExprTrue  "AND" ExprTrue  -> ExprTrue
    ExprBinary ExprTrue  "AND" ExprFalse -> ExprFalse
    ExprBinary ExprFalse "AND" ExprTrue  -> ExprFalse
    ExprBinary ExprFalse "AND" ExprFalse -> ExprFalse
    ExprBinary e1 "AND" ExprFalse -> ExprFalse
    ExprBinary e1 "AND" ExprTrue  -> simplify e1
    ExprBinary ExprFalse "AND" e2 -> ExprFalse
    ExprBinary ExprTrue  "AND" e2 -> simplify e2

    ExprBinary ExprTrue  "OR" ExprTrue  -> ExprTrue
    ExprBinary ExprTrue  "OR" ExprFalse -> ExprTrue
    ExprBinary ExprFalse "OR" ExprTrue  -> ExprTrue
    ExprBinary ExprFalse "OR" ExprFalse -> ExprFalse
    ExprBinary e1 "OR" ExprFalse -> simplify e1
    ExprBinary e1 "OR" ExprTrue  -> ExprTrue
    ExprBinary ExprFalse "OR" e2 -> simplify e2
    ExprBinary ExprTrue  "OR" e2 -> ExprTrue

    ExprBinary e1 "&" (ExprIntegerConst  0) -> ExprIntegerConst  0
    ExprBinary (ExprIntegerConst  0) "&" e2 -> ExprIntegerConst  0

    ExprBinary e1 "|" (ExprIntegerConst  0) -> simplify e1
    ExprBinary (ExprIntegerConst  0) "|" e2 -> simplify e2

    ExprBinary e1 "^" (ExprIntegerConst  0) -> simplify e1
    ExprBinary (ExprIntegerConst  0) "^" e2 -> simplify e2

    ExprBinary (ExprIntegerConst c1) "="  (ExprIntegerConst c2) -> exprBool (c1 == c2)
    ExprBinary (ExprDoubleConst  c1) "="  (ExprDoubleConst  c2) -> exprBool (c1 == c2)
    ExprBinary (ExprStringConst  c1) "="  (ExprStringConst  c2) -> exprBool (c1 == c2)

    ExprBinary (ExprIntegerConst c1) "!=" (ExprIntegerConst c2) -> exprBool (c1 /= c2)
    ExprBinary (ExprDoubleConst  c1) "!=" (ExprDoubleConst  c2) -> exprBool (c1 /= c2)
    ExprBinary (ExprStringConst  c1) "!=" (ExprStringConst  c2) -> exprBool (c1 /= c2)

    ExprBinary (ExprIntegerConst c1) "<"  (ExprIntegerConst c2) -> exprBool (c1 < c2)
    ExprBinary (ExprDoubleConst  c1) "<"  (ExprDoubleConst  c2) -> exprBool (c1 < c2)
    ExprBinary (ExprStringConst  c1) "<"  (ExprStringConst  c2) -> exprBool (c1 < c2)

    ExprBinary (ExprIntegerConst c1) ">"  (ExprIntegerConst c2) -> exprBool (c1 > c2)
    ExprBinary (ExprDoubleConst  c1) ">"  (ExprDoubleConst  c2) -> exprBool (c1 > c2)
    ExprBinary (ExprStringConst  c1) ">"  (ExprStringConst  c2) -> exprBool (c1 > c2)

    ExprBinary (ExprIntegerConst c1) "<=" (ExprIntegerConst c2) -> exprBool (c1 <= c2)
    ExprBinary (ExprDoubleConst  c1) "<=" (ExprDoubleConst  c2) -> exprBool (c1 <= c2)
    ExprBinary (ExprStringConst  c1) "<=" (ExprStringConst  c2) -> exprBool (c1 <= c2)

    ExprBinary (ExprIntegerConst c1) ">=" (ExprIntegerConst c2) -> exprBool (c1 >= c2)
    ExprBinary (ExprDoubleConst  c1) ">=" (ExprDoubleConst  c2) -> exprBool (c1 >= c2)
    ExprBinary (ExprStringConst  c1) ">=" (ExprStringConst  c2) -> exprBool (c1 >= c2)

    ExprUnary op e1 -> if s1 == e1
        then expr
        else simplify (ExprUnary op s1)
      where
        s1 = simplify e1

    ExprBinary e1 op e2 -> if (s1 == e1) && (s2 == e2)
        then expr
        else simplify (ExprBinary s1 op s2)
      where
        s1 = simplify e1
        s2 = simplify e2

    _ -> expr
  where
    exprBool True  = ExprTrue
    exprBool False = ExprFalse








-- Не поддерживается FULL OUTER JOIN и NATURAL JOIN.
-- Expression может быть using
data From
    = FromAs String From -- ^ задать псевдоним
    | FromTable Table
    | CrossJoin From From
    | InnerJoin From From Expression
    | LeftJoin  From From Expression
    | RightJoin From From Expression
    | FromSelect Select
    deriving (Eq, Show)

instance ToText From where
    toText (FromAs alias f) = "FromAs '" ++ alias ++ "' {\n" ++ indented (toText f) ++ "\n}"
    toText (FromTable tab) = "FromTable '" ++ getName tab ++ "'"
    toText (CrossJoin f1 f2) = "CrossJoin {" ++ toText f1 ++ " * " ++ toText f2 ++ "}"
    toText (InnerJoin f1 f2 expr) = "InnerJoin {" ++ toText f1 ++ " * " ++ toText f2 ++ " ON " ++ toText expr ++ "}"
    toText (LeftJoin  f1 f2 expr) = "LeftJoin {"  ++ toText f1 ++ " * " ++ toText f2 ++ " ON " ++ toText expr ++ "}"
    toText (RightJoin f1 f2 expr) = "RightJoin {" ++ toText f1 ++ " * " ++ toText f2 ++ " ON " ++ toText expr ++ "}"
    toText (FromSelect sel) = "FromSelect {\n" ++ toText sel ++ "\n}"

instance HasTables From where
    innerTables f = case f of
        FromAs _ f1          -> innerTables f1
        FromTable tab        -> [tab]
        CrossJoin f1 f2      -> innerTables f1 ++ innerTables f2
        InnerJoin f1 f2 expr -> innerTables f1 ++ innerTables f2 ++ innerTables expr
        LeftJoin  f1 f2 expr -> innerTables f1 ++ innerTables f2 ++ innerTables expr
        RightJoin f1 f2 expr -> innerTables f1 ++ innerTables f2 ++ innerTables expr
        FromSelect sel       -> innerTables sel
    innerRecords f = case f of
        FromAs _ f1          -> innerRecords f1
        FromTable tab        -> []
        CrossJoin f1 f2      -> innerRecords f1 ++ innerRecords f2
        InnerJoin f1 f2 expr -> innerRecords f1 ++ innerRecords f2 ++ innerRecords expr
        LeftJoin  f1 f2 expr -> innerRecords f1 ++ innerRecords f2 ++ innerRecords expr
        RightJoin f1 f2 expr -> innerRecords f1 ++ innerRecords f2 ++ innerRecords expr
        FromSelect sel       -> innerRecords sel


-- |Запрос SELECT.
data Select = Select {
    select'name     :: String,
    select'comment  :: [String],
    select'params   :: Record,
    select'result   :: Record,
    select'binds    :: Map.Map String Expression,
    select'from     :: From,
    select'where    :: Expression,
    select'distinct :: Bool,
    select'groupBy  :: [Order],
    select'having   :: Expression,
    select'orderBy  :: [Order],
    select'limit    :: Expression, -- NULL, если нет предела
    select'offset   :: Expression  -- 0, если нет смещения
}   deriving (Eq, Show)

instance ToText Select where
    toText x = "Select " ++ show (getName x) ++ " {\n"
        ++ showComment x
        ++ indent ++ "Params = '" ++ getName (select'params x) ++ "'\n"
        ++ indent ++ "Result = '" ++ getName (select'result x) ++ "'\n"
        ++ indented ("Binds = [\n" ++ Map.foldlWithKey' bindToText "" (select'binds x)) ++ "]\n"
        ++ indented ("From = " ++ toText (select'from x)) ++ "\n"
        ++ indented ("Where = " ++ toText (select'where x)) ++ "\n"
        ++ indent ++ "Distinct = " ++ show (select'distinct x) ++ "\n"
        ++ "}"
      where
        bindToText :: String -> String -> Expression -> String
        bindToText s nm ex = s ++ indent ++ nm ++ " = " ++ toText ex ++ "\n"

instance HasTables Select where
    innerTables sel = innerTables (select'from sel)
        ++ innerTables (select'where sel)
        ++ innerTables (select'having sel)
        ++ innerTables (select'limit sel)
        ++ innerTables (select'offset sel)
        ++ concat (map innerTables (Map.elems (select'binds sel)))
    innerRecords sel = [select'result sel, select'params sel]
        ++ innerRecords (select'from sel)
        ++ innerRecords (select'where sel)
        ++ innerRecords (select'having sel)
        ++ innerRecords (select'limit sel)
        ++ innerRecords (select'offset sel)
        ++ concat (map innerRecords (Map.elems (select'binds sel)))


select :: From -> Record -> Select
select frm res = Select{
    select'name     = "select",
    select'comment  = [],
    select'params   = record,
    select'result   = res,
    select'binds    = Map.empty,
    select'from     = frm,
    select'where    = ExprTrue,
    select'distinct = False,
    select'groupBy  = [],
    select'having   = ExprTrue,
    select'orderBy  = [],
    select'limit    = ExprNull,
    select'offset   = ExprIntegerConst 0
}

instance HasName Select where
    name s r = r{ select'name = s }
    getName = select'name
    getTitle it = "select '" ++ select'name it ++ "'"

instance HasComment Select where
    comment ss it = it{ select'comment = ss }
    getComment = select'comment

class HasParams it where
    params :: Record -> it -> it
    getParams :: it -> Record

    nameParams :: String -> it -> it
    nameParams s query = params (getParams query #name s) query

    -- addParam :: Field -> it -> it
    -- addParam fld query = params (getParams query #addParam fld) query

instance HasParams Select where
    params pars sel = sel{ select'params = pars }
    getParams = select'params


getResult :: Select -> Record
getResult = select'result


bind :: [Link String Expression] -> Select -> Select
bind [] sel = sel
bind [Link fld expr] sel = sel { select'binds = Map.insert fld expr old }
  where old = select'binds sel
bind (x:xs) sel = sel #bind [x] #bind xs

getBinds :: Select -> Map.Map String Expression
getBinds = select'binds

getBind :: Select -> String -> Maybe Expression
getBind sel fld = Map.lookup fld (select'binds sel)


getFrom :: Select -> From
getFrom = select'from


class HasWhere it where
    where_ :: Expression -> it -> it
    getWhere :: it -> Expression
    
    notWhere :: it -> it
    notWhere x = where_ (not_ $ getWhere x) x

    andWhere :: Expression -> it -> it
    andWhere expr x = where_ (getWhere x .&&. expr) x

    orWhere :: Expression -> it -> it
    orWhere expr x = where_ (getWhere x .||. expr) x

instance HasWhere Select where
    where_ w sel = sel{ select'where = simplify w }
    getWhere = select'where


distinct :: Select -> Select
distinct sel = sel{ select'distinct = True }

getDistinct :: Select -> Bool
getDistinct = select'distinct


groupBy :: [Order] -> Select -> Select
groupBy grb sel = sel{ select'groupBy = grb }

getGroupBy :: Select -> [Order]
getGroupBy = select'groupBy


having :: Expression -> Select -> Select
having w sel = sel{ select'having = simplify w }

getHaving :: Select -> Expression
getHaving = select'having


orderBy :: [Order] -> Select -> Select
orderBy orb sel = sel{ select'orderBy = orb }

getOrderBy :: Select -> [Order]
getOrderBy = select'orderBy


limit_ :: Expression -> Select -> Select
limit_ expr sel = sel{ select'limit = simplify expr }

limit :: Integer -> Select -> Select
limit lim sel = sel{ select'limit = ExprIntegerConst lim }

noLimit :: Select -> Select
noLimit sel = sel{ select'limit = ExprNull }

getLimit :: Select -> Expression
getLimit = select'limit


offset_ :: Expression -> Select -> Select
offset_ expr sel = sel{ select'offset = simplify expr }

offset :: Integer -> Select -> Select
offset ofs sel = sel{ select'offset = ExprIntegerConst ofs }

getOffset :: Select -> Expression
getOffset = select'offset


-- |Чтение одной записи таблицы по первичному ключу.
readOne :: Table -> Select
readOne tab = select (FromTable tab) (getRecord tab)
    #name   ("ReadOne_" ++ getName tab)
    #params (primaryKeyRecord tab)
    #where_ (whereGiven tab primaryKey)
    // ("Read one record from '" ++ getName tab ++ "' table by primary key.")
  where
    primaryKey = getKey tab

-- |Чтение всей таблицы, упорядоченной по первичному ключу.
readAll :: Table -> Select
readAll tab = select (FromTable tab) (getRecord tab)
    #name    ("ReadAll_" ++ getName tab)
    #orderBy (map Asc primaryKey)
    // ("Read all records from '" ++ getName tab ++ "' table ordered by primary key.")
  where
    primaryKey = getKey tab

whereGiven :: Table -> [String] -> Expression
whereGiven tab []  = ExprTrue
whereGiven tab [x] = ExprField tab x .==. ExprParam x t
  where
    t = case getField x tab of
      Nothing  -> error $ "Primary key field '" ++ x ++ "' not found in whereGiven"
      Just fld -> getType fld
whereGiven tab (x:xs) = whereGiven tab [x] .&&. whereGiven tab xs

-- |Переименовать структуру возвращаемых данных.
returnAs :: String -> Select -> Select
returnAs rn sel = sel{ select'result = sr }
  where
    sr = select'result sel #name rn

-- |Урезать структуру возвращаемых данных.
--  Структура переименовывается автоматически.
returnOnly :: [String] -> Select -> Select
returnOnly xs sel = sel{ select'result = sr }
  where
    oldRes = select'result sel
    sr = selectFromRecord xs oldRes
        #name (getName oldRes ++ "_" ++ concat xs)


























