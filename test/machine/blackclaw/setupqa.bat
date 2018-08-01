@echo off
rem
rem	File:		setupqa.bat
rem
rem	Usage:		$ setupqa.bat
rem
rem	Function:	To setup env variables needed to QA WISP
rem			using ACUCOBOL 
rem
@echo on

set WISPSERVER=(LOCAL)
set WISPDIR=D:\work\shellstream\WISP\src\QA\wisp
set WISPCONFIG=D:\work\shellstream\WISP\src\test\config
set WISPDEBUG=FULL
set WISPSHAREDIR=D:\work\shellstream\WISP\src\test\message
set WISPTMPDIR=D:\work\shellstream\WISP\src\test\temp
set WISPLINKPATH=D:\work\shellstream\WISP\src\QA\WISP\bin;D:\work\shellstream\WISP\src\QA\WISP\acu;D:\work\shellstream\WISP\src\test;D:\work\shellstream\WISP\src\test\VOLRUN\ONPATH;D:\work\shellstream\WISP\src\QA\KCSIACU
set PATH=%WISPDIR%\bin;%PATH%
rem set VCOLORS=0A0CA0ACEAECBABCA0C00ACAAECEABCB
