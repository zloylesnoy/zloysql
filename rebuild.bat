rmdir /S /Q .stack-work
del /Q _*.txt
@echo .
@echo =======================================================================
@echo ===========================   REBUILD.BAT   ===========================
stack build
stack exec zloysql-exe
pause
