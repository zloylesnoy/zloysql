module Main where

import Sql
import Sp


sqls :: Scheme -> DialectSQL -> String
sqls sch lang = case check lang sch of
    [] -> concat $ map (\s -> s ++ "\n\n") (sqlCreate lang sch ++ sqlDrop lang sch ++ sqlQueries lang sch)
    _  -> concat $ map (\s -> s ++ "\n"  ) (check lang sch)

main :: IO ()
main = do
    writeFile "_show.txt"  (show spScheme)
    writeFile "_mysql.txt" (sqls spScheme MySQL)
    writeFile "_mssql.txt" (sqls spScheme MicrosoftSQL)
    writeFile "_pgsql.txt" (sqls spScheme PostgreSQL)

























