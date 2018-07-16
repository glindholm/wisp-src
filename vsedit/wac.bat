@ECHO OFF
REM ########################################################################
REM #
REM #               WISP - Wang Interchange Source Pre-processor
REM #      Copyright (c) Shell Stream Software LLC, All rights reserved.
REM #
REM ########################################################################
REM #
REM #       wac     WISP and COMPILE
REM #
REM #       Usage:  wac (basename) (ext) (objectdir)
REM #
REM #       This script is used by VSEDIT to WISP and COMPILE a COBOL
REM #       program.  It can also be used as a general script for doing
REM #       the above.  It first must be customized for the users
REM #       environment, see below.
REM #
REM #       basename        The file path of the file to be translated
REM #                       by WISP.  It can be an absolute or relative
REM #                       path. It must not include an extension.
REM #
REM #       ext             The extension for basename
REM #
REM #       objectdir       The directory to place the final COBOL object
REM #                       files. If ommitted then "." is assumed.  If a
REM #                       relative path is given it is taken relative to
REM #                       $filepath.
REM #
REM #
REM #
REM #       NOTES:
REM #		1) This file does NOT redirect stdout or stderr as this
REM #		   will be done by VSEDIT.
REM #
REM #
REM #
REM ########################################################################
REM #       The following variables must be configured by the user:
REM #
REM #       WISPCMD         This name of the wisp translator
REM #
REM #       WISPFLAGS       Flags to use with WISP
REM #
REM #       COBOL           Name of the COBOL compiler
REM #       COBFLAGS        Flags to use with the COBOL compiler
REM #
REM #       OBJFILE         The name of the object produced by the COBOL
REM #                       compiler.
REM #
REM #
REM ########################################################################

SET INFILE=%1%
SET WCBEXT=%2%
SET OBJDIR=%3%

SET WCBFILE=%INFILE%.%WCBEXT%
SET COBFILE=%INFILE%.cob
SET OBJFILE=%INFILE%.acu

SET WISPCMD=WISP.exe
SET WISPFLAGS= -VACU

SET COBOL=CCBL32.exe
SET COBFLAGS=-da4 -Dg -Za -o %OBJFILE%

IF EXIST %WCBFILE% GOTO WISP
ECHO "# FILE %WCBFILE% NOT FOUND"
GOTO END

:WISP
IF EXIST %COBFILE% DEL %COBFILE%
ECHO "# WISPING %WCBFILE%"
@ECHO ON
%WISPCMD% %WISPFLAGS% %WCBFILE%
@ECHO OFF
IF EXIST %COBFILE% GOTO COBOL
ECHO "# FILE %COBFILE% NOT FOUND"
GOTO END

:COBOL
IF EXIST %OBJFILE% DEL %OBJFILE%
ECHO "# COMPILING %COBFILE%"
@ECHO ON
%COBOL% %COBFLAGS% %COBFILE%
@ECHO OFF
IF EXIST %OBJFILE% COPY %OBJFILE% %OBJDIR%

:END
