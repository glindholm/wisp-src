# Microsoft Developer Studio Project File - Name="proctran" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=proctran - Win32 Release
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "proctran.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "proctran.mak" CFG="proctran - Win32 Release"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "proctran - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "proctran - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "proctran - Win32 Release"

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
# ADD CPP /nologo /MD /W3 /GX /O2 /I "..\wispcommon" /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /YX /FD /c
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386 /out:"..\bin\proctran.exe"
# SUBTRACT LINK32 /incremental:yes

!ELSEIF  "$(CFG)" == "proctran - Win32 Debug"

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
# ADD CPP /nologo /MDd /W3 /Gm /GX /ZI /Od /I "..\wispcommon" /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /YX /FD /c
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /incremental:no /debug /machine:I386 /out:"..\bin\proctran.exe"

!ENDIF 

# Begin Target

# Name "proctran - Win32 Release"
# Name "proctran - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;hpj;bat;for;f90"
# Begin Source File

SOURCE=..\wispcommon\getopt.c
# End Source File
# Begin Source File

SOURCE=.\pgcblsrc.h
# End Source File
# Begin Source File

SOURCE=.\pgcommon.h
# End Source File
# Begin Source File

SOURCE=.\pgdefeqn.h
# End Source File
# Begin Source File

SOURCE=.\pgeqtns.h
# End Source File
# Begin Source File

SOURCE=.\pgextrct.h
# End Source File
# Begin Source File

SOURCE=.\pgglobal.h
# End Source File
# Begin Source File

SOURCE=.\pgkeyw.h
# End Source File
# Begin Source File

SOURCE=.\pgstruct.h
# End Source File
# Begin Source File

SOURCE=.\ptcheck.c
# End Source File
# Begin Source File

SOURCE=.\ptcli.c
# End Source File
# Begin Source File

SOURCE=.\ptdecl.c
# End Source File
# Begin Source File

SOURCE=.\ptdoit.c
# End Source File
# Begin Source File

SOURCE=.\ptequatn.c
# End Source File
# Begin Source File

SOURCE=.\ptextrct.c
# End Source File
# Begin Source File

SOURCE=.\ptif.c
# End Source File
# Begin Source File

SOURCE=.\ptinit.c
# End Source File
# Begin Source File

SOURCE=.\ptmain.c
# End Source File
# Begin Source File

SOURCE=.\ptpdiv.c
# End Source File
# Begin Source File

SOURCE=.\ptrens.c
# End Source File
# Begin Source File

SOURCE=.\ptscrdef.c
# End Source File
# Begin Source File

SOURCE=.\ptscreen.c
# End Source File
# Begin Source File

SOURCE=.\pttype.c
# End Source File
# Begin Source File

SOURCE=.\ptusing.c
# End Source File
# Begin Source File

SOURCE=.\ptutils.c
# End Source File
# Begin Source File

SOURCE=.\ptwrite.c
# End Source File
# Begin Source File

SOURCE=..\wispcommon\ring.c
# End Source File
# Begin Source File

SOURCE=..\wispcommon\sleepdos.c
# End Source File
# Begin Source File

SOURCE=..\wispcommon\wisp_pic.c
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl;fi;fd"
# Begin Source File

SOURCE=..\wispcommon\getopt.h
# End Source File
# Begin Source File

SOURCE=..\wispcommon\ring.h
# End Source File
# Begin Source File

SOURCE=..\wispcommon\wisp_pic.h
# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;cnt;rtf;gif;jpg;jpeg;jpe"
# End Group
# End Target
# End Project
