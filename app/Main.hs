module Main where

import Sql
import GoLang
import Sp
import GHC.IO.Encoding


sqls :: DialectSQL -> Scheme -> String
sqls lang sch = case check lang sch of
    [] -> concat $ map (\s -> s ++ "\n\n") (sqlCreate lang sch ++ sqlDrop lang sch ++ sqlQueries lang sch)
    _  -> concat $ map (\s -> s ++ "\n"  ) (check lang sch)

main :: IO ()
main = do
    setLocaleEncoding utf8
    writeFile "_sp_.txt"  (toText spScheme)
    writeFile "_mysql.txt" (sqls MySQL spScheme)
    writeFile "_mssql.txt" (sqls MicrosoftSQL spScheme)
    writeFile "_pgsql.txt" (sqls PostgreSQL spScheme)
    writeFile "_zloysql.go" (golang MySQL spScheme)

























