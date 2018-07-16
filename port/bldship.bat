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
REM	File:		bldship.bat
REM
REM	Function:	This batch builds the WISP ship kit on MSDOS
REM
REM	Desciption:	This routine is run after WISP has been built
REM			on the machine.  It builds the temporary shipping
REM			kit area under the "src" directory and moves all
REM			the needed components into it.  Once this temp
REM			shipping kit has been tested it would be renamed
REM			and moved out of the "src" directory to become
REM			the real shipping kit.
REM
REM	Input:		%WISP%		The WISP base directory.
REM			%WISP%\src\...	The ported WISP.
REM
REM	Output:		%WISP%\src\ship\...
REM					The temporary ship kit.
REM
REM	History:	06/09/92	Written by GSL
REM			06/12/92	Add ilpremote/ilpsrv, wshell, make.include. GSL
REM			06/30/92	Add wdiag. GSL
REM			07/08/92	Add info messages. GSL
REM			07/08/92	Add the loading of $SHIP/rts. GSL
REM			01/19/93	Modified for MSDOS
REM
if not "x%WISP%" == "x" goto lab1 
echo Variable WISP is not set!
echo bldship.bat ABORTING!
goto theexit
:lab1
REM
REM	Define some variables
REM
set SRC=%WISP%\SRC
set SHIP=%SRC%\SHIP
set WU=%SRC%\WISPUTIL
set PT=%SRC%\PROCTRAN
set VT=%SRC%\VIDEOTEST
set VC=%SRC%\VIDEOCAP
set WT=%SRC%\WISPTRAN
set WL=%SRC%\WISPLIB
set VL=%SRC%\VIDEOLIB
set ETC=%SRC%\ETC
set PORT=%SRC%\PORT
set WACU=%SRC%\ACU
set EDE=%SRC%\EDE
REM
REM	Create all the SHIP KIT directories
REM
echo Creating %SHIP%
@echo on
mkdir %SHIP%
mkdir %SHIP%\wisp
mkdir %SHIP%\wisp\bin
mkdir %SHIP%\wisp\lib
mkdir %SHIP%\wisp\etc
mkdir %SHIP%\wisp\acu
mkdir %SHIP%\wisp\config
mkdir %SHIP%\wisp\config\videocap
mkdir %SHIP%\ede
mkdir %SHIP%\ede\demo
mkdir %SHIP%\crid
@echo off
REM
REM	@Copy all the files into the ship kit
REM
echo Loading %SHIP%\wisp\bin
@echo on
copy %WU%\display.exe		%SHIP%\wisp\bin
copy %WU%\makemsg.exe		%SHIP%\wisp\bin
copy %PT%\proctran.exe		%SHIP%\wisp\bin
copy %VT%\vtest.exe		%SHIP%\wisp\bin
copy %SRC%\VSEDIT\vsedit.exe	%SHIP%\wisp\bin
copy %WU%\wcopy.exe		%SHIP%\wisp\bin
copy %WT%\wisp.exe		%SHIP%\wisp\bin
copy %WU%\wlicense.exe		%SHIP%\wisp\bin
copy %WU%\wputparm.exe		%SHIP%\wisp\bin
copy %WU%\wrun.exe		%SHIP%\wisp\bin
copy %WU%\wshell.bat		%SHIP%\wisp\bin
copy %WU%\wsort.exe		%SHIP%\wisp\bin
copy %WU%\wusage.exe		%SHIP%\wisp\bin
copy %WACU%\wruncbl.exe		%SHIP%\wisp\bin
@echo Loading %SHIP%\wisp\lib
copy %VL%\video.lib		%SHIP%\wisp\lib
copy %WL%\wisp.lib		%SHIP%\wisp\lib
@echo Loading %SHIP%\wisp\etc
copy %ETC%\RELNOTES		%SHIP%\wisp\etc
copy %WU%\disprint.wcb		%SHIP%\wisp\etc
copy %WU%\disprint.mak		%SHIP%\wisp\etc
copy %WT%\words.def		%SHIP%\wisp\etc
copy %PORT%\dosmake.mak		%SHIP%\wisp\etc
copy %VC%\vblue.bat		%SHIP%\wisp\etc
copy %VC%\vbrown.bat		%SHIP%\wisp\etc
copy %VC%\vcyan.bat		%SHIP%\wisp\etc
copy %VC%\vgreen.bat		%SHIP%\wisp\etc
copy %VC%\vpink.bat		%SHIP%\wisp\etc
copy %VC%\vred.bat		%SHIP%\wisp\etc
copy %VC%\vwhite.bat		%SHIP%\wisp\etc
copy %VC%\vyellow.bat		%SHIP%\wisp\etc
cd %ETC%
%WU%\makemsg
cd %PORT%
@echo Loading %SHIP%\wisp\config
copy %ETC%\lgmap.dos		%SHIP%\wisp\config\LGMAP
copy %ETC%\options.dat		%SHIP%\wisp\config\OPTIONS
copy %ETC%\prtmap.dat		%SHIP%\wisp\config\PRTMAP
copy %ETC%\wispmsg.dat		%SHIP%\wisp\config
copy %ETC%\wispmsg.txt		%SHIP%\wisp\config
copy %ETC%\wrun.cfg		%SHIP%\wisp\config
copy %ETC%\wsysconf.cfg		%SHIP%\wisp\config
@echo Loading %SHIP%\wisp\config\videocap
@REM copy the videocap files cutting off the .VCA extension
copy %VC%\*.VCA		%SHIP%\wisp\config\videocap\*
@echo Loading %SHIP%\wisp\acu
copy %WACU%\acudos.rul		%SHIP%\wisp\acu
copy %WACU%\acuusing.cob	%SHIP%\wisp\acu
copy %WACU%\sub85.c		%SHIP%\wisp\acu
copy %WACU%\wruncbl.mak		%SHIP%\wisp\acu
@echo Loading %SHIP%\ede
copy %EDE%\good.exe		%SHIP%\ede
copy %EDE%\ede.lib		%SHIP%\ede
copy %WACU%\wruncble.exe	%SHIP%\ede
copy %EDE%\helpmap.dat		%SHIP%\ede\demo\HELPMAP
copy %EDE%\*.wcb		%SHIP%\ede\demo
copy %EDE%\*.hlp		%SHIP%\ede\demo
copy %EDE%\menudemo.opt		%SHIP%\ede\demo
copy %EDE%\menudemo.umf		%SHIP%\ede\demo\menudemo.mak
@echo off
echo The SHIP KIT has been built.
echo If there were any errors reported then they must
echo be investigated before continuing.
:theexit
