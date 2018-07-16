@echo off
REM/************************************************************************/
REM/*									 */
REM/*		 @Copyright (c) 1993					 */
REM/*	 An unpublished work of International Digital Scientific Inc.	 */
REM/*			    All rights reserved.			 */
REM/*									 */
REM/************************************************************************/
REM
REM
REM	File:		CFGACU.BAT
REM
REM	Function:	This batch builds the testacu $WISPCONFIG
REM
REM	Desciption:	This routine is run after WISP has been built
REM			on the machine.  It builds the $WISPCONFIG
REM			directory for testing ACUCOBOL on MSDOS with CB.
REM
REM	Input:		%WISP%		The WISP base directory.
REM			%WISP%\src\...	The ported WISP.
REM
REM	Output:		%WISP%\src\testacu\config\...
REM					The WISP config directory
REM
REM	History:	03/23/93	Written by GSL
REM
set SCRIPT=CFGACU.BAT
if not "x%WISP%" == "x" goto lab1 
echo Variable WISP is not set!
echo %SCRIPT% ABORTING!
goto theexit
:lab1
REM
REM	Define some variables
REM
set SRC=%WISP%\SRC
set TESTDIR=%SRC%\testacu
set VC=%SRC%\VIDEOCAP
set ETC=%SRC%\ETC
set WISPCONFIG=%TESTDIR%\config
REM
REM	Create all the WISPCONFIG directories
REM
echo Creating %WISPCONFIG%
mkdir %WISPCONFIG%
mkdir %WISPCONFIG%\videocap

REM
REM	@Copy all the files into the ship kit
REM
echo Loading %WISPCONFIG%
copy %ETC%\wispmsg.dat	%WISPCONFIG%
copy %ETC%\wispmsg.txt	%WISPCONFIG%
copy %ETC%\wrun.cfg	%WISPCONFIG%
copy %ETC%\wsysconf.cfg	%WISPCONFIG%
copy %ETC%\options.dat	%WISPCONFIG%\OPTIONS
copy %TESTDIR%\lgmap.dac %WISPCONFIG%\LGMAP
REM
REM
echo Loading %WISPCONFIG%\videocap
REM copy the videocap files cutting off the .VCA extension
copy %VC%\*.VCA		%WISPCONFIG%\videocap\*
REM
REM
echo The WISPCONFIG %WISPCONFIG% directory has been built.
echo The following files are still needed:
echo	ACUCOBOL.CFG	($ACU\CBLCONFI)
echo	LGMAP		(need modification)
echo
:theexit
