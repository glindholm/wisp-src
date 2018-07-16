@echo off
if exist pkunzip.exe goto make_kit
@echo	*******************************************************************
@echo		ERROR: Could not find pkunzip.exe in SHIP directory
@echo			EXITING BATCH FILE
@echo	*******************************************************************
goto exit
:make_kit
@echo	*******************************************************************
@echo		Place a formatted 3.5" floppy in drive b:, press ENTER
@echo			when ready
@echo	*******************************************************************
pause
@echo on
copy pkunzip.exe b:\
copy readme.ede b:\readme.txt
pkzip -rP& b:\ede.zip ede\*.*
@echo off
:exit
REM cleanup and leave

