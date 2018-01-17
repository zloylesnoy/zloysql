module Main where

import Sql
import GoLang
import Sp
import GHC.IO.Encoding


sqls :: Scheme -> DialectSQL -> String
sqls sch lang = case check lang sch of
    [] -> concat $ map (\s -> s ++ "\n\n") (sqlCreate lang sch ++ sqlDrop lang sch ++ sqlQueries lang sch)
    _  -> concat $ map (\s -> s ++ "\n"  ) (check lang sch)

main :: IO ()
main = do
    setLocaleEncoding utf8
    writeFile "_sp_.txt"  (toText spScheme)
    writeFile "_mysql.txt" (sqls spScheme MySQL)
    writeFile "_mssql.txt" (sqls spScheme MicrosoftSQL)
    writeFile "_pgsql.txt" (sqls spScheme PostgreSQL)
    writeFile "_zloysql.go" (golang spScheme)

























