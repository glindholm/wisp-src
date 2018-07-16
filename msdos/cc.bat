@echo off
rem	CC.BAT - Compile C source file in Micro Soft C 6.00 
rem			with IDSI system options.
rem
rem				Look for any argument:
IF %1. == . GOTO NOARG
rem				Look for argument %1.C file:
IF EXIST %1.C GOTO ADDEXT
rem				Source.C not found, move name to SRC variable:
SET SRC=%1
rem				Look for source file with out adding extension:
IF EXIST %1 GOTO COMPILE
rem				Source file not found, abort:
ECHO "%1 File not found."
SET SRC=
GOTO THEEND

:ADDEXT
rem				Use source.C for source file name.
SET SRC=%1.C
rem				Do the compile.
GOTO COMPILE

:COMPILE
rem				Compile using IDSI specific options.
CL /AL /c /Zi /Gt3 /nologo /G2 %SRC%
SET SRC=
GOTO THEEND

:NOARG
ECHO "No argument: this routine requires a source file name argument."
GOTO THEEND
:THEEND
