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
    HasParams, params, getParams,
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

    readOne, readAll, whereGiven, returnAs, returnOnly,
    sqlExpr, sqlSelect
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

    deriving (Eq)

instance Show Expression where
    show ExprNull = "NULL"
    show ExprFalse = "FALSE"
    show ExprTrue = "TRUE"
    show (ExprIntegerConst x) = show x
    show (ExprDoubleConst x) = show x
    show (ExprDecimalConst digs prec) = showDecimal digs prec
    show (ExprStringConst x) = show x
    show (ExprUnary oper e1) = "(" ++ oper ++ show e1 ++ ")"
    show (ExprBinary e1 oper e2) = "(" ++ show e1 ++ " " ++ oper ++ " " ++ show e2 ++ ")"
    show (ExprParam nm tp) = "{$" ++ nm ++ "::" ++ getName tp ++ "}"
    show (ExprField tab nm) = getName tab ++ "." ++ nm
    show (ExprSelect sel) = "(\n" ++ show sel ++ "\n)"
    show (ExprOuter alias nm tp) = "{^" ++ alias ++ "." ++ nm ++ "::" ++ getName tp ++ "}"
    show (ExprJoinUsing xs) = "{USING " ++ show xs ++ "}"
    show ExprDefault = "DEFAULT"
    show (ExprSet var val) = show var ++ " = " ++ show val

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

sqlExpr :: DialectSQL -> Expression -> String
sqlExpr lang expr = case simplify expr of

    ExprNull  -> "NULL"
    ExprTrue  -> "(0 = 0)"
    ExprFalse -> "(0 = 1)"
    ExprIntegerConst c  -> if c < 0
        then "(" ++ show c ++ ")"
        else show c
    ExprDoubleConst c   -> if c < 0
        then "(" ++ show c ++ ")"
        else show c
    ExprDecimalConst digits precision -> show digits -- TODO
    ExprStringConst c -> escaped lang c

    -- |Унарный оператор.
    ExprUnary oper e1 -> "(" ++ oper ++ sqlExpr lang e1 ++ ")"

    -- |Бинарный оператор.
    ExprBinary e1 "^" e2 -> let
        xor = if lang == PostgreSQL then "#" else "^"
      in bin e1 xor e2
    ExprBinary e1 oper e2 -> bin e1 oper e2

    -- Используют контекст. TODO
    ExprParam param t -> " @" ++ param ++ " "
    ExprField tab fld -> quotedId lang (getName tab) ++ "." ++ quotedId lang fld
    ExprSelect sel -> "(\n" ++ sqlSelect lang sel ++ "\n)"
    ExprOuter alias fld t -> quotedId lang alias ++ "." ++ quotedId lang fld
    ExprJoinUsing flds -> "USING " ++ join ", " (map (quotedId lang) flds)
    ExprDefault -> "DEFAULT"
    ExprSet e1 e2 -> sqlExpr lang e1 ++ " = " ++ sqlExpr lang e2
  where
    bin = \e1 op e2 ->
        "(" ++ sqlExpr lang e1 ++ " " ++ op ++ " " ++ sqlExpr lang e2 ++ ")"
    






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
    deriving (Eq)

instance Show From where
    show (FromAs alias f) = "FromAs '" ++ alias ++ "' {\n" ++ indented (show f) ++ "\n}"
    show (FromTable tab) = "FromTable '" ++ getName tab ++ "'"
    show (CrossJoin f1 f2) = "CrossJoin {" ++ show f1 ++ " * " ++ show f2 ++ "}"
    show (InnerJoin f1 f2 expr) = "InnerJoin {" ++ show f1 ++ " * " ++ show f2 ++ " ON " ++ show expr ++ "}"
    show (LeftJoin  f1 f2 expr) = "LeftJoin {"  ++ show f1 ++ " * " ++ show f2 ++ " ON " ++ show expr ++ "}"
    show (RightJoin f1 f2 expr) = "RightJoin {" ++ show f1 ++ " * " ++ show f2 ++ " ON " ++ show expr ++ "}"
    show (FromSelect sel) = "FromSelect {\n" ++ show sel ++ "\n}"

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
}   deriving (Eq)

instance Show Select where
    show x = "Select " ++ show (getName x) ++ " {\n"
        ++ sqlComment x
        ++ indent ++ "Params = '" ++ getName (select'params x) ++ "'\n"
        ++ indent ++ "Result = '" ++ getName (select'result x) ++ "'\n"
        ++ indented ("Binds = [" ++ sBinds (Map.toList $ select'binds x)) ++ "]\n"
        ++ indented ("From = " ++ show (select'from x)) ++ "\n"
        ++ indented ("Where = " ++ show (select'where x)) ++ "\n"
        ++ indent ++ "Distinct = " ++ show (select'distinct x) ++ "\n"
        ++ "}"
      where
        sBinds [] = ""
        sBinds (x:xs) = show x ++ ", " ++ sBinds xs

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
    #name   ("readOne_" ++ getName tab)
    #params (primaryKeyRecord tab)
    #where_ (whereGiven tab primaryKey)
    // ("Read one record from '" ++ getName tab ++ "' table by primary key.")
  where
    primaryKey = getKey tab

-- |Чтение всей таблицы, упорядоченной по первичному ключу.
readAll :: Table -> Select
readAll tab = select (FromTable tab) (getRecord tab)
    #name    ("readAll_" ++ getName tab)
    #orderBy (map Asc primaryKey)
    // ("Read all records from '" ++ getName tab ++ "' table ordered by primary key.")
  where
    primaryKey = getKey tab

whereGiven :: Table -> [String] -> Expression
whereGiven tab []  = ExprTrue
whereGiven tab [x] = ExprField tab x .==. ExprParam x t
  where
    t = case getField x tab of
      Nothing  -> error "Primary key field not found in whereGiven"
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


-- | Возвращает секцию LIMIT ... OFFSET ... оператора SQL SELECT.
sqlLimit :: DialectSQL -> Expression -> Expression -> String

sqlLimit MySQL lim ofs = if (ofs == ExprIntegerConst 0) && (lim == ExprNull) then ""
    -- [LIMIT row_count OFFSET offset]
    else "\nLIMIT " ++ sqlExpr MicrosoftSQL lim ++ "\nOFFSET " ++ sqlExpr MicrosoftSQL ofs

sqlLimit MicrosoftSQL lim ofs = ofset ++ limit
    -- [OFFSET число ROWS [FETCH FIRST число ROWS ONLY]]
  where
    ofset = if (ofs == ExprIntegerConst 0) && (lim == ExprNull) then ""
        else "\nOFFSET " ++ sqlExpr MicrosoftSQL ofs ++ " ROWS"
    limit = if lim == ExprNull then ""
        else "\nFETCH FIRST " ++ sqlExpr MicrosoftSQL lim ++ " ROWS ONLY"

sqlLimit PostgreSQL lim ofs = limit ++ ofset
    -- [ LIMIT число ] [ OFFSET число ]
  where
    limit = if lim == ExprNull then ""
        else "\nLIMIT " ++ sqlExpr PostgreSQL lim
    ofset = if ofs == ExprIntegerConst 0 then ""
        else "\nOFFSET " ++ sqlExpr PostgreSQL ofs


sqlOrder :: DialectSQL -> Order -> String
sqlOrder lang (Asc  fld) = quotedId lang fld ++ " ASC"
sqlOrder lang (Desc fld) = quotedId lang fld ++ " DESC"

sqlOrders :: DialectSQL -> String -> [Order] -> String
sqlOrders _ _ []      = ""
sqlOrders lang hdr xs = hdr ++ join ", " (map (sqlOrder lang) xs)


-- | Возвращает секцию SELECT ... FROM оператора SQL SELECT.
sqlSelectResult :: DialectSQL -> Select -> Field -> String
sqlSelectResult lang sel fld = case fldExpr of
    -- Nothing   -> error "Field not found in select'binds"
    Nothing   -> quotedId lang fldName
    Just expr -> (sqlExpr lang expr) ++ " AS " ++ quotedId lang fldName
  where
    fldName = getName fld
    fldExpr = Map.lookup fldName (select'binds sel) -- :: Maybe Expression


sqlOn :: DialectSQL -> Expression -> String
sqlOn _ ExprTrue = ""
sqlOn lang expr  = " ON " ++ sqlExpr lang expr


-- | Возвращает секцию FROM ... оператора SQL SELECT.
sqlFrom :: DialectSQL -> From -> String

sqlFrom lang (FromAs alias frm) = sqlFrom lang frm ++ " AS " ++ quotedId lang alias

sqlFrom lang (FromTable tab) = quotedId lang $ getName tab

sqlFrom lang (CrossJoin f1 f2) = "(" ++ sqlFrom lang f1 ++ " CROSS JOIN " ++ sqlFrom lang f2 ++ ")"

sqlFrom lang (InnerJoin f1 f2 expr) = "(" ++ sqlFrom lang f1 ++
    " INNER JOIN " ++ sqlFrom lang f2 ++ sqlOn lang expr ++ ")"

sqlFrom lang (LeftJoin f1 f2 expr) = "(" ++ sqlFrom lang f1 ++
    " LEFT JOIN " ++ sqlFrom lang f2 ++ sqlOn lang expr ++ ")"

sqlFrom lang (RightJoin f1 f2 expr) = "(" ++ sqlFrom lang f1 ++
    " RIGHT JOIN " ++ sqlFrom lang f2 ++ sqlOn lang expr ++ ")"

sqlFrom lang (FromSelect sel) = sqlSelect lang sel


-- |Возвращает оператор SQL SELECT.
sqlSelect :: DialectSQL -> Select -> String
sqlSelect lang sel = "SELECT" ++ sDistinct ++ "\n  " ++ sResult ++ "\nFROM " ++ sFrom
    ++ sWhere ++ sGroupBy ++ sHaving ++ sOrderBy ++ sLimit
  where
    sDistinct = if getDistinct sel then " DISTINCT" else ""
    sFrom     = sqlFrom lang (select'from sel)
    sWhere    = let wr = getWhere sel in
        if wr == ExprTrue then "" else "\nWHERE " ++ sqlExpr lang wr
    sHaving   = let hv = select'having sel in
        if hv == ExprTrue then "" else "\nHAVING " ++ sqlExpr lang hv
    sGroupBy  = sqlOrders lang "\nGROUP BY " (select'groupBy sel)
    sOrderBy  = sqlOrders lang "\nORDER BY " (select'orderBy sel)
    sLimit    = sqlLimit lang (select'limit sel) (select'offset sel)
    sResult   = join ",\n  " $ map (sqlSelectResult lang sel) (getFields $ select'result sel)























