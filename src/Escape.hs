module Escape (
    quotedId, quotedIds, escaped
) where 

import Data.Bits ((.&.), shiftR)
import qualified Data.Set as Set
import Data.String.Utils (join)

import Common


-- |Quote identifier. Identifier must be goodId.
quotedId :: Language -> String -> String
quotedId lang id = case lang of
    MySQL        -> "`"  ++ id ++ "`"
    MicrosoftSQL -> "\"" ++ id ++ "\""
    PostgreSQL   -> "\"" ++ id ++ "\""

-- |List of quoted identifiers. Identifiers must be goodId.
quotedIds :: Language -> [String] -> String
quotedIds lang ids = join ", " (map (quotedId lang) ids)


-- |Quote string using escape-secuences.
escaped :: Language -> String -> String

escaped MySQL s = "'" ++ myEscaped s ++ "'"

escaped PostgreSQL s = if pges == s
    then "'" ++ s ++ "'"
    else "U&'" ++ pges ++ "'"
  where
    pges = pgEscaped s

escaped MicrosoftSQL s = "'" ++ myEscaped s ++ "'" -- TODO


-- |This symbols escaped in MySql.Data.MySqlClient.MySqlHelper.EscapeString (C#).
myEscapedChars :: Set.Set Char
myEscapedChars = Set.fromList [
    '\x0022', '\x0027', '\x005C', '\x0060', '\x00A5', '\x00B4', '\x0160', '\x02B9',
    '\x02BA', '\x02BB', '\x02BC', '\x02C8', '\x02CA', '\x02CB', '\x02D9', '\x0300',
    '\x0301', '\x2018', '\x2019', '\x201A', '\x2032', '\x2035', '\x20A9', '\x2216',
    '\x275B', '\x275C', '\xFE68', '\xFF07', '\xFF3C']

-- |Escape characters for MySQL.
myEscaped :: String -> String
myEscaped [] = []
myEscaped (x:xs) = if Set.member x myEscapedChars
    then '\\' : x : myEscaped xs
    else x : myEscaped xs

-- |Set of characters escaped in PostgreSQL strings.
pgEscapedChars :: Set.Set Char
pgEscapedChars = myEscapedChars -- TODO

-- |Escape characters for PostgreSQL.
pgEscaped :: String -> String
pgEscaped [] = []
pgEscaped ('\\':xs) = '\\' : '\\' : pgEscaped xs
pgEscaped (x:xs) = if Set.member x pgEscapedChars
    then pgEscCh x ++ pgEscaped xs
    else x : pgEscaped xs

-- |Escape one character for PostgreSQL.
pgEscCh :: Char -> String
pgEscCh ch = if n < 0x10000
    then ['\\', d3, d2, d1, d0]
    else ['\\', '+', d5, d4, d3, d2, d1, d0]
  where
    n = fromEnum ch
    d0 = hex (n .&. 15)
    d1 = hex (shiftR n  4 .&. 15)
    d2 = hex (shiftR n  8 .&. 15)
    d3 = hex (shiftR n 12 .&. 15)
    d4 = hex (shiftR n 16 .&. 15)
    d5 = hex (shiftR n 20 .&. 15)

-- |Decimal 0..15 to hexadecimal character.
hex :: Int -> Char
hex 0 = '0'
hex 1 = '1'
hex 2 = '2'
hex 3 = '3'
hex 4 = '4'
hex 5 = '5'
hex 6 = '6'
hex 7 = '7'
hex 8 = '8'
hex 9 = '9'
hex 10 = 'A'
hex 11 = 'B'
hex 12 = 'C'
hex 13 = 'D'
hex 14 = 'E'
hex 15 = 'F'
hex x = error $ "Error in 'hex " ++ show x ++ "' function."


















