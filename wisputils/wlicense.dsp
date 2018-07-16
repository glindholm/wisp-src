# Microsoft Developer Studio Project File - Name="wlicense" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=wlicense - Win32 Release
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "wlicense.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "wlicense.mak" CFG="wlicense - Win32 Release"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "wlicense - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "wlicense - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "wlicense - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir ".\Release"
# PROP BASE Intermediate_Dir ".\Release"
# PROP BASE Target_Dir "."
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir ".\Release"
# PROP Intermediate_Dir ".\Release"
# PROP Target_Dir "."
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /YX /c
# ADD CPP /nologo /MD /W3 /GX /O2 /I "..\wispcommon" /D "NDEBUG" /D "WIN32" /D "_CONSOLE" /D "MSFS" /FR /YX /FD /c
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386 /out:"..\bin\wlicense.exe"
# SUBTRACT LINK32 /incremental:yes

!ELSEIF  "$(CFG)" == "wlicense - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir ".\Debug"
# PROP BASE Intermediate_Dir ".\Debug"
# PROP BASE Target_Dir "."
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir ".\Debug"
# PROP Intermediate_Dir ".\Debug"
# PROP Target_Dir "."
# ADD BASE CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /YX /c
# ADD CPP /nologo /MDd /W3 /Gm /GX /ZI /Od /I "..\wispcommon" /D "_DEBUG" /D "WIN32" /D "_CONSOLE" /D "MSFS" /YX /FD /c
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /incremental:no /debug /machine:I386 /out:"..\bin\wlicense.exe"

!ENDIF 

# Begin Target

# Name "wlicense - Win32 Release"
# Name "wlicense - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;hpj;bat;for;f90"
# Begin Source File

SOURCE=..\wisplib\fexists.c
# End Source File
# Begin Source File

SOURCE=..\wisplib\idsisubs.c
# End Source File
# Begin Source File

SOURCE=..\wisplib\licwisp.c
# End Source File
# Begin Source File

SOURCE=..\wisplib\machid.c
# End Source File
# Begin Source File

SOURCE=..\wisplib\makepath.c
# End Source File
# Begin Source File

SOURCE=..\wisplib\platsubs.c
# End Source File
# Begin Source File

SOURCE=..\wisputils\prompt.c
# End Source File
# Begin Source File

SOURCE=..\wisplib\setenvst.c
# End Source File
# Begin Source File

SOURCE=..\wisplib\wanguid.c
# End Source File
# Begin Source File

SOURCE=..\wisplib\wassert.c
# End Source File
# Begin Source File

SOURCE=..\wisplib\wauthsub.c
# End Source File
# Begin Source File

SOURCE=..\wisplib\wgetpgrp.c
# End Source File
# Begin Source File

SOURCE=..\wisplib\wglobals.c
# End Source File
# Begin Source File

SOURCE=..\wisplib\winnt.c
# End Source File
# Begin Source File

SOURCE=..\wisplib\wispcfg.c
# End Source File
# Begin Source File

SOURCE=..\wisplib\wispvers.c
# End Source File
# Begin Source File

SOURCE=..\wisputils\wlicense.c
# End Source File
# Begin Source File

SOURCE=..\wisplib\wlickey.c
# End Source File
# Begin Source File

SOURCE=..\wisplib\wmalloc.c
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl;fi;fd"
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;cnt;rtf;gif;jpg;jpeg;jpe"
# End Group
# End Target
# End Project
