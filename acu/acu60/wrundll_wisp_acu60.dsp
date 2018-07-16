# Microsoft Developer Studio Project File - Name="wrundll" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

CFG=wrundll - Win32 Release
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "wrundll.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "wrundll.mak" CFG="wrundll - Win32 Release"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "wrundll - Win32 Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe
# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir ""
# PROP Intermediate_Dir ""
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "WRUNDLL_EXPORTS" /YX /FD /c
# ADD CPP /nologo /MD /W3 /Ox /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "_WINDLL" /FD /c
# SUBTRACT CPP /YX
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /machine:I386 /out:"wrun32.dll"
# SUBTRACT LINK32 /pdb:none /debug
# Begin Target

# Name "wrundll - Win32 Release"
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

SOURCE=.\sub.c
# End Source File
# Begin Source File

SOURCE=.\sub85.c
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=.\wrundll.def
# End Source File
# Begin Source File

SOURCE=.\wrundll.rc
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

SOURCE=.\acucobol.ico
# End Source File
# Begin Source File

SOURCE=.\acucobol2.ico
# End Source File
# Begin Source File

SOURCE=.\acudebug.ico
# End Source File
# Begin Source File

SOURCE=.\acudebug2.ico
# End Source File
# Begin Source File

SOURCE=.\divider.cur
# End Source File
# Begin Source File

SOURCE=.\go.cur
# End Source File
# Begin Source File

SOURCE=.\help.cur
# End Source File
# Begin Source File

SOURCE=.\wispicon.ico
# End Source File
# End Group
# Begin Source File

SOURCE=.\zlib.lib
# End Source File
# Begin Source File

SOURCE=.\atermmgr.lib
# End Source File
# Begin Source File

SOURCE=.\avision5.lib
# End Source File
# Begin Source File

SOURCE=.\wcvt32.lib
# End Source File
# Begin Source File

SOURCE=.\wfsi32.lib
# End Source File
# Begin Source File

SOURCE=.\acme.lib
# End Source File
# Begin Source File

SOURCE=.\wrunlib.lib
# End Source File
# Begin Source File

SOURCE=.\wisp.lib
# End Source File
# Begin Source File

SOURCE=.\video.lib
# End Source File
# End Target
# End Project
