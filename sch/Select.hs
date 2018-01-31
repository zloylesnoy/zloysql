module Select (
    HasInnerTables, innerTables, innerRecords,

    Expression(..), simplify,
    using, not_, inv, neg, exists, not_exists,
    (~.),
    (.+.), (.-.), (.*.), (./.), (.%.),
    (.&&.), (.||.), (.&.), (.|.), xor,
    (.==.), (.!=.), (.<.), (.>.), (.<=.), (.>=.),
    (.==.?), (.!=.?), (.<.?), (.>.?), (.<=.?), (.>=.?),
    (.==.*), (.!=.*), (.<.*), (.>.*), (.<=.*), (.>=.*),
    SetExpression(..), (.=.),

    From (..), Select, select,
    HasParams, params, getParams, nameParams,
    getFrom,
    HasWhere, where_, getWhere, notWhere, andWhere, orWhere,
    HasResult, getResult,
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
class HasInnerTables it where
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

    -- |Параметр запроса (имя параметра).
    | ExprParam String

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

    deriving (Eq, Show)

infixl 9 ~.
(~.) :: Table -> String -> Expression
(~.) tab col = ExprField tab col

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
    toText (ExprParam nm) = "{$" ++ nm ++ "}"
    toText (ExprField tab nm) = getName tab ++ "." ++ nm
    toText (ExprSelect sel) = "(\n" ++ toText sel ++ "\n)"
    toText (ExprOuter alias nm tp) = "{^" ++ alias ++ "." ++ nm ++ "::" ++ getName tp ++ "}"
    toText (ExprJoinUsing xs) = "{USING " ++ show xs ++ "}"
    toText ExprDefault = "DEFAULT"

instance HasInnerTables Expression where
    innerTables expr = case expr of
        ExprUnary _ e1     -> innerTables e1
        ExprBinary e1 _ e2 -> innerTables e1 ++ innerTables e2
        ExprField tab _    -> [tab]
        ExprSelect sel     -> innerTables sel
        _                  -> []
    innerRecords expr = case expr of
        ExprUnary _ e1     -> innerRecords e1
        ExprBinary e1 _ e2 -> innerRecords e1 ++ innerRecords e2
        ExprSelect sel     -> innerRecords sel
        _                  -> []


using :: [String] -> Expression
using ss = ExprJoinUsing ss

-- |Logical NOT.
not_ :: Expression -> Expression
not_ e1 = ExprUnary "NOT" e1

-- |Bitwise NOT.
inv :: Expression -> Expression
inv e1 = ExprUnary "~" e1

-- |Change sign.
neg :: Expression -> Expression
neg e1 = ExprUnary "-" e1

exists :: Expression -> Expression
exists e1 = ExprUnary "EXISTS" e1

not_exists :: Expression -> Expression
not_exists e1 = ExprUnary "NOT EXISTS" e1

-- |Add two numbers or concatenate two strings.
infixl 6 .+.
(.+.) :: Expression -> Expression -> Expression
e1 .+. e2 = ExprBinary e1 "+" e2

-- |Subtraction.
infixl 6 .-.
(.-.) :: Expression -> Expression -> Expression
e1 .-. e2 = ExprBinary e1 "-" e2

-- |Multiplication.
infixl 7 .*.
(.*.) :: Expression -> Expression -> Expression
e1 .*. e2 = ExprBinary e1 "*" e2

-- |Division.
infixl 7 ./.
(./.) :: Expression -> Expression -> Expression
e1 ./. e2 = ExprBinary e1 "/" e2

-- |Reminder.
infixl 7 .%.
(.%.) :: Expression -> Expression -> Expression
e1 .%. e2 = ExprBinary e1 "%" e2

-- |Logical AND.
infixr 3 .&&.
(.&&.) :: Expression -> Expression -> Expression
e1 .&&. e2 = ExprBinary e1 "AND" e2

-- |Logical OR.
infixr 2 .||.
(.||.) :: Expression -> Expression -> Expression
e1 .||. e2 = ExprBinary e1 "OR" e2

-- |Bitwise AND.
infixr 7 .&.
(.&.) :: Expression -> Expression -> Expression
e1 .&. e2 = ExprBinary e1 "&" e2

-- |Bitwise OR.
infixr 5 .|.
(.|.) :: Expression -> Expression -> Expression
e1 .|. e2 = ExprBinary e1 "|" e2

-- |Bitwise XOR.
infixr 6 `xor`
xor :: Expression -> Expression -> Expression
xor e1 e2 = ExprBinary e1 "^" e2


-- |Equals.
infixl 4 .==.
(.==.) :: Expression -> Expression -> Expression
e1 .==. e2 = ExprBinary e1 "=" e2

-- |Not equals.
infixl 4 .!=.
(.!=.) :: Expression -> Expression -> Expression
e1 .!=. e2 = ExprBinary e1 "!=" e2

-- |Less than.
infixl 4 .<.
(.<.) :: Expression -> Expression -> Expression
e1 .<. e2 = ExprBinary e1 "<" e2

-- |Greater than.
infixl 4 .>.
(.>.) :: Expression -> Expression -> Expression
e1 .>. e2 = ExprBinary e1 ">" e2

-- |Less or equals.
infixl 4 .<=.
(.<=.) :: Expression -> Expression -> Expression
e1 .<=. e2 = ExprBinary e1 "<=" e2

-- |Greater or equals.
infixl 4 .>=.
(.>=.) :: Expression -> Expression -> Expression
e1 .>=. e2 = ExprBinary e1 ">=" e2


-- |Equals any of.
infixl 4 .==.?
(.==.?) :: Expression -> Expression -> Expression
e1 .==.? e2 = ExprBinary e1 "= ANY" e2

-- |Not equals any of.
infixl 4 .!=.?
(.!=.?) :: Expression -> Expression -> Expression
e1 .!=.? e2 = ExprBinary e1 "!= ANY" e2

-- |Less than any.
infixl 4 .<.?
(.<.?) :: Expression -> Expression -> Expression
e1 .<.? e2 = ExprBinary e1 "< ANY" e2

-- |Greater than any.
infixl 4 .>.?
(.>.?) :: Expression -> Expression -> Expression
e1 .>.? e2 = ExprBinary e1 "> ANY" e2

-- |Less or equals any.
infixl 4 .<=.?
(.<=.?) :: Expression -> Expression -> Expression
e1 .<=.? e2 = ExprBinary e1 "<= ANY" e2

-- |Greater or equals any.
infixl 4 .>=.?
(.>=.?) :: Expression -> Expression -> Expression
e1 .>=.? e2 = ExprBinary e1 ">= ANY" e2


-- |Equals all.
infixl 4 .==.*
(.==.*) :: Expression -> Expression -> Expression
e1 .==.* e2 = ExprBinary e1 "= ALL" e2

-- |Not equals all.
infixl 4 .!=.*
(.!=.*) :: Expression -> Expression -> Expression
e1 .!=.* e2 = ExprBinary e1 "!= ALL" e2

-- |Less than all.
infixl 4 .<.*
(.<.*) :: Expression -> Expression -> Expression
e1 .<.* e2 = ExprBinary e1 "< ALL" e2

-- |Greater than all.
infixl 4 .>.*
(.>.*) :: Expression -> Expression -> Expression
e1 .>.* e2 = ExprBinary e1 "> ALL" e2

-- |Less or equals all.
infixl 4 .<=.*
(.<=.*) :: Expression -> Expression -> Expression
e1 .<=.* e2 = ExprBinary e1 "<= ALL" e2

-- |Greate or equals all.
infixl 4 .>=.*
(.>=.*) :: Expression -> Expression -> Expression
e1 .>=.* e2 = ExprBinary e1 ">= ALL" e2


data SetExpression = SetExpression String Expression
    deriving (Eq, Show)

instance HasInnerTables SetExpression where
    innerTables  (SetExpression s e) = innerTables  e
    innerRecords (SetExpression s e) = innerRecords e

-- |Assignment in the SET section of UPDATE query.
infixl 2 .=.
(.=.) :: String -> Expression -> SetExpression
s .=. e2 = SetExpression s e2

instance ToText SetExpression where
    toText (SetExpression s e) = s ++ " = " ++ toText e


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








-- FULL OUTER JOIN and NATURAL JOIN not supported.
-- Expression can be USING.
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

instance HasInnerTables From where
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


-- |SQL SELECT query.
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

instance HasInnerTables Select where
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


select :: Record -> From -> Select
select res frm = Select{
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

class HasResult it where
    getResult :: it -> Record

instance HasResult Select where
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


-- |Read one record by primary key.
readOne :: Table -> Select
readOne tab = select (getRecord tab) (FromTable tab)
    #name   ("ReadOne_" ++ getName tab)
    #params (primaryKeyRecord tab)
    #where_ (whereGiven tab primaryKey)
    // ("Read one record from '" ++ getName tab ++ "' table by primary key.")
  where
    primaryKey = getKey tab

-- |Read all data from the table ordered by primary key.
readAll :: Table -> Select
readAll tab = select (getRecord tab) (FromTable tab)
    #name    ("ReadAll_" ++ getName tab)
    #orderBy (map Asc primaryKey)
    // ("Read all records from '" ++ getName tab ++ "' table ordered by primary key.")
  where
    primaryKey = getKey tab

whereGiven :: Table -> [String] -> Expression
whereGiven tab []     = ExprTrue
whereGiven tab [x]    = ExprField tab x .==. ExprParam x
whereGiven tab (x:xs) = whereGiven tab [x] .&&. whereGiven tab xs

-- |Rename returned record.
returnAs :: String -> Select -> Select
returnAs rn sel = sel{ select'result = sr }
  where
    sr = select'result sel #name rn

-- |Reduce returned data and rename returned record.
returnOnly :: [String] -> Select -> Select
returnOnly xs sel = sel{ select'result = sr }
  where
    oldRes = select'result sel
    sr = selectFromRecord xs oldRes
        #name (getName oldRes ++ "_" ++ concat xs)


























