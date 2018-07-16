@echo off
rem     CCLIB.BAT - Compile C source file in Micro Soft C 6.00 
rem                     with IDSI system options.
rem                     and add to a library.
rem
rem                             Look for any argument:
IF %1. == . GOTO NOARG
rem                             Compile using IDSI specific options.
CALL CC %2
IF %3. == ADD. GOTO ADDIT
LIB /NOL /NOI %1 -+%2.OBJ;
GOTO THEEND

:ADDIT
LIB /NOL /NOI %1 +%2.OBJ;
GOTO THEEND

:NOARG
ECHO "No argument: this routine requires a source file name argument."
GOTO THEEND
:THEEND
