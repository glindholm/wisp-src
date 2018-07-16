@echo off
REM
REM     Bind one exe to the 4GWPRO memory manager
REM
if exist xxx.exe del xxx.exe
4gwbind 4gwpro.exe %1% xxx.exe -q -v
if exist xxx.exe move xxx.exe %1%
REM
REM     Done
REM


