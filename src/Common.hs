﻿module Common (
    (//), (#),
    Language (..), isSQL,
    Order(..), orderFieldName,

    indent, indented,
    pow2x, pow10x, showDecimal,
    notUniques, goodId, goodIdWithDots,
    Errors,

    HasCheck, check, checkLanguages,
    HasName, getName, name, getTitle, errorIn, checkName,
    HasComment, getComment, comment, addComment, showComment
) where 

import Text.Regex.Posix ((=~))
import Data.String.Utils (join, split)
-- import Data.Scientific (Scientific)


--  Adds one comment line.
infixl 1 //

-- |Helps with the fluent interface.
infixl 1 #
a # f = f a

-- |Programming languages for generated code.
data Language
    = MySQL
    | MicrosoftSQL
    | PostgreSQL
    | GoLang
    | CSharp
    | CLang
    | CPlusPlus
    deriving (Eq)

instance Show Language where
    show MySQL        = "MySQL"
    show MicrosoftSQL = "Microsoft SQL"
    show PostgreSQL   = "PostgreSQL"
    show GoLang       = "Go language"
    show CSharp       = "C# language"
    show CLang        = "C language"
    show CPlusPlus    = "C++ language"

-- |Is this language a SQL dialect.
isSQL :: Language -> Bool
isSQL lang = case lang of
    MySQL        -> True
    MicrosoftSQL -> True
    PostgreSQL   -> True
    _            -> False


-- |Sort order for ORDER BY, HAVING and INDEX.
data Order
    = Asc  String -- ^ Ascending order.
    | Desc String -- ^ Descending order.
    deriving (Eq, Show)

orderFieldName :: Order -> String
orderFieldName (Asc  x) = x
orderFieldName (Desc x) = x


-- |Indentation step for generated code.
indent = "    "

-- |Add indentation to multiline code.
indented :: String -> String
indented s = indent ++ join ('\n' : indent) (split "\n" s)

-- |Power (2, x).
pow2x :: Int -> Integer
pow2x x = (2 :: Integer) ^ x

-- |Power (10, x).
pow10x :: Int -> Integer
pow10x x = (10 :: Integer) ^ x

showDecimal :: Integer -> Int -> String
showDecimal digs prec = show d ++ "." ++ tail(show(one + m))
  where
    one = pow10x prec
    (d, m) = divMod digs one

{-
showScientific :: Sci.Scientific -> String
showScientific sc = 
  where
    (ddd, e) = Sci.toDecimalDigits (abs sc)
    n = length ddd -- digits
    scAfter  = max 0 (n - e) -- digits after dot
    scDigits = max scAfter (scAfter + e) -- digits with additional zeroes at the end
    scBefore = scDigits - scAfter   -- digits before dot
    scZeroes = max 0 (scBefore - n) -- add zeroes before dot
-}
{-
    0.1234E-2 = 0.001234 n=4 e=-2 digits=6 after=6 before=0 zeroes=0
    0.1234E+0 = 0.1234   n=4 e=0  digits=4 after=4 before=0 zeroes=0
    0.1234E+2 = 12.34    n=4 e=2  digits=4 after=2 before=2 zeroes=0
    0.1234E+4 = 1234     n=4 e=4  digits=4 after=0 before=4 zeroes=0
    0.1234E+6 = 123400   n=4 e=6  digits=6 after=0 before=6 zeroes=2
-}



-- |Найти неуникальные имена.
--  TODO: case insensitive
notUniques :: [String] -> [String]
notUniques [] = []
notUniques [x] = []
notUniques (x:xs) = if (elem x xs) && not (elem x nuxs)
    then x:nuxs
    else nuxs
  where
    nuxs = notUniques xs

-- |Строка является правильным идентификатором.
--  MySQL ограничивает длину идентификаторов 64 символами.
goodId :: String -> Bool
goodId id = (id =~ "^[a-zA-Z_][a-zA-Z_0-9]*$") && (length id < 64)

-- |Строка является правильным идентификатором с точками.
goodIdWithDots :: String -> Bool
goodIdWithDots id = id =~ "^[a-zA-Z_][a-zA-Z_0-9]*([.][a-zA-Z_][a-zA-Z_0-9]*)*$"


-- |Результат проверки это список однострочных описаний ошибок.
--  Если проверка прошла успешно, возвращаем пустой список ошибок.
type Errors = [String]

-- |Entitle error list with some title.
entitleErrors :: String -> Errors -> Errors
entitleErrors title [] = []
entitleErrors title errors = title : map (indent ++) errors

-- |Something we can check.
class HasCheck it where
    -- |Check for one languages.
    check :: Language -> it -> Errors

    -- |Check for every language in list.
    checkLanguages :: [Language] -> it -> Errors
    checkLanguages langs x = concat $ map (checkL x) langs
      where
        checkL x lang = entitleErrors ("Code generation failed for " ++ show lang) (check lang x)

-- |Something with name.
class HasName it where
    getName :: it -> String

    -- |Set name, fluent interface.
    name :: String -> it -> it

    -- |Get name for error messages.
    getTitle :: it -> String

    -- |Entitle error list with object name.
    errorIn :: it -> Errors -> Errors
    errorIn x errors = entitleErrors ("Error in " ++ getTitle x) errors

    -- |Is name a valid identifier.
    checkName :: it -> Errors
    checkName x = if goodId s then [] else ["Invalid name '" ++ s ++ "'"]
        where s = getName x

-- |Something with multiline comment.
class HasComment it where
    getComment :: it -> [String]

    -- |Set comment, fluent interface.
    comment :: [String] -> it -> it
   
    -- |Add comment line.
    addComment :: String -> it -> it
    addComment s x = comment (old ++ [s]) x where old = getComment x
   
    -- |Add one comment line, fluent interface.
    (//) :: it -> String -> it
    (//) x s = addComment s x

    -- |Generate code for comment.
    showComment :: Language -> it -> String
    showComment lang x = concat (map (\s -> prefix ++ s ++ "\n") (getComment x))
        where prefix = if isSQL lang then "-- " else "// "





















