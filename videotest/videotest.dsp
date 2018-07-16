# Microsoft Developer Studio Project File - Name="videotest" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=videotest - Win32 Release
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "videotest.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "videotest.mak" CFG="videotest - Win32 Release"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "videotest - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "videotest - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "videotest - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir ".\Release"
# PROP BASE Intermediate_Dir ".\Release"
# PROP BASE Target_Dir "."
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir ".\Release"
# PROP Intermediate_Dir ".\Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir "."
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /YX /c
# ADD CPP /nologo /MD /W3 /GX /O2 /I "..\videolib" /D "NDEBUG" /D "WIN32" /D "_CONSOLE" /D "WINNT" /D "MSFS" /YX /FD /c
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /map /machine:I386 /out:"..\bin\vtest.exe"
# SUBTRACT LINK32 /incremental:yes

!ELSEIF  "$(CFG)" == "videotest - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir ".\Debug"
# PROP BASE Intermediate_Dir ".\Debug"
# PROP BASE Target_Dir "."
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir ".\Debug"
# PROP Intermediate_Dir ".\Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir "."
# ADD BASE CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /YX /c
# ADD CPP /nologo /MDd /W3 /Gm /Gi /GX /ZI /Od /I "..\videolib" /D "_DEBUG" /D "WIN32" /D "_CONSOLE" /D "WINNT" /D "MSFS" /YX /FD /c
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /incremental:no /map /debug /machine:I386 /out:"..\bin\vtestd.exe"

!ENDIF 

# Begin Target

# Name "videotest - Win32 Release"
# Name "videotest - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;hpj;bat;for;f90"
# Begin Source File

SOURCE=.\dump_map.c
# End Source File
# Begin Source File

SOURCE=.\vtest0.c
# End Source File
# Begin Source File

SOURCE=.\vtesta.c
# End Source File
# Begin Source File

SOURCE=.\vtestb.c
# End Source File
# Begin Source File

SOURCE=.\vtestc.c
# End Source File
# Begin Source File

SOURCE=.\vtestd.c
# End Source File
# Begin Source File

SOURCE=.\vteste.c
# End Source File
# Begin Source File

SOURCE=.\vtestf.c
# End Source File
# Begin Source File

SOURCE=.\vtestg.c
# End Source File
# Begin Source File

SOURCE=.\vtesth.c
# End Source File
# Begin Source File

SOURCE=.\vtesti.c
# End Source File
# Begin Source File

SOURCE=.\vtestj.c
# End Source File
# Begin Source File

SOURCE=.\vtestk.c
# End Source File
# Begin Source File

SOURCE=.\vtestl.c
# End Source File
# Begin Source File

SOURCE=.\vtestm.c
# End Source File
# Begin Source File

SOURCE=.\vtestn.c
# End Source File
# Begin Source File

SOURCE=.\vtesto.c
# End Source File
# Begin Source File

SOURCE=.\vtestp.c
# End Source File
# Begin Source File

SOURCE=.\vtestq.c
# End Source File
# Begin Source File

SOURCE=.\vtestr.c
# End Source File
# Begin Source File

SOURCE=.\vtests.c
# End Source File
# Begin Source File

SOURCE=.\vtestt.c
# End Source File
# Begin Source File

SOURCE=.\vtestu.c
# End Source File
# Begin Source File

SOURCE=.\vtestv.c
# End Source File
# Begin Source File

SOURCE=.\vtestw.c
# End Source File
# Begin Source File

SOURCE=.\vtestx.c
# End Source File
# Begin Source File

SOURCE=.\vtesty.c
# End Source File
# Begin Source File

SOURCE=.\vtestz.c
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
