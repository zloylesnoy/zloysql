del /Q _*.txt
del /Q _*.go
@echo .
@echo =======================================================================
stack build
stack exec zloysql-exe
copy _zloysql.go   C:\WORK\Go\src\blackbox\blmemo\zloysql\zloysql.go
copy gen\common.go C:\WORK\Go\src\blackbox\blmemo\zloysql\common.go
