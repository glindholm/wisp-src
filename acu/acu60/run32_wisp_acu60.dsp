# Microsoft Developer Studio Project File - Name="run32" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=run32 - Win32 Release
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "run32_wisp.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "run32_wisp.mak" CFG="RUN32 - WIN32 RELEASE"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "run32 - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
RSC=rc.exe
# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "run32___Win32_Release"
# PROP BASE Intermediate_Dir "run32___Win32_Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "run32___Win32_Release"
# PROP Intermediate_Dir "run32___Win32_Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /MD /W3 /GX /Zi /Ox /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /D "_WINDOWS" /FR /FD /c
# SUBTRACT CPP /YX
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 setargv advapi32.lib user32.lib gdi32.lib winspool.lib Comdlg32.lib /nologo /subsystem:console /machine:I386 /out:"run32_wisp.exe"
# SUBTRACT LINK32 /debug
# Begin Target

# Name "run32 - Win32 Release"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=.\config85.c
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=.\direct.c
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=.\filetbl.c
# End Source File
# Begin Source File

SOURCE=.\mswinsub.c
# End Source File
# Begin Source File

SOURCE=.\run32.rc
# End Source File
# Begin Source File

SOURCE=.\sub.c
# End Source File
# Begin Source File

SOURCE=.\sub85.c
# PROP Exclude_From_Build 1
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Source File

SOURCE=.\sub.h
# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# Begin Source File

SOURCE=.\run.ico
# End Source File
# Begin Source File

SOURCE=.\run2.ico
# End Source File
# Begin Source File

SOURCE=.\wispicon.ico
# End Source File
# End Group
# Begin Source File

SOURCE=.\wfsi32.lib
# End Source File
# Begin Source File

SOURCE=.\avision5.lib
# End Source File
# Begin Source File

SOURCE=.\wcvt32.lib
# End Source File
# Begin Source File

SOURCE=.\acme.lib
# End Source File
# Begin Source File

SOURCE=.\crunlib.lib
# End Source File
# Begin Source File

SOURCE=.\term32.lib
# End Source File
# Begin Source File

SOURCE=.\video.lib
# End Source File
# Begin Source File

SOURCE=.\wisp.lib
# End Source File
# End Target
# End Project
