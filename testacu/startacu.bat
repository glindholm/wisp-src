@echo off
rem
rem	File:		startup.dac
rem
rem	Usage:		$ startup.dac
rem
rem	Function:	To setup env variables needed to QA WISP
rem			using ACUCOBOL 386 on MSDOS.
rem
@echo on
set QA=%WISP%\SRC\TESTACU
set WISPCONFIG=%QA%\CONFIG
set A_CONFIG=%WISPCONFIG%\ACUCOBOL.CFG
set USER=GregLindholm
set HOME=%QA%
