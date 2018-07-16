# Microsoft Developer Studio Project File - Name="createlibacu" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=createlibacu - Win32 Release
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "createlibacu.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "createlibacu.mak" CFG="createlibacu - Win32 Release"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "createlibacu - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "createlibacu - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "createlibacu - Win32 Release"

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
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /W3 /GX /O2 /I "..\..\..\wispcommon" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "WINNT" /D "MSFS" /D "KCSI_ACU" /D "KCSI_WIN32" /D CREATE_VERSION=34 /YX /FD /c
# ADD BASE RSC /l 0x409
# ADD RSC /l 0x409
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo /out:"createacu.lib"

!ELSEIF  "$(CFG)" == "createlibacu - Win32 Debug"

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
# ADD BASE CPP /nologo /W3 /GX /Z7 /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /W3 /GX /Z7 /Od /I "..\..\..\wispcommon" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "WINNT" /D "MSFS" /D "KCSI_ACU" /D "KCSI_WIN32" /D CREATE_VERSION=34 /YX /FD /c
# ADD BASE RSC /l 0x409
# ADD RSC /l 0x409
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo /out:"createacu.lib"

!ENDIF 

# Begin Target

# Name "createlibacu - Win32 Release"
# Name "createlibacu - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;hpj;bat;for;f90"
# Begin Source File

SOURCE=brlio.c
# End Source File
# Begin Source File

SOURCE=ccsioerr.c
# End Source File
# Begin Source File

SOURCE=cridebug.c
# End Source File
# Begin Source File

SOURCE=gp.c
# End Source File
# Begin Source File

SOURCE=kexists.c
# End Source File
# Begin Source File

SOURCE=kv3.c
# End Source File
# Begin Source File

SOURCE=ll.c
# End Source File
# Begin Source File

SOURCE=seqio.c
# End Source File
# Begin Source File

SOURCE=valflv.c
# End Source File
# Begin Source File

SOURCE=vcsio.c
# End Source File
# Begin Source File

SOURCE=version.c
# End Source File
# Begin Source File

SOURCE=vscrbchk.c
# End Source File
# Begin Source File

SOURCE=vscrblk.c
# End Source File
# Begin Source File

SOURCE=vscrdbg.c
# End Source File
# Begin Source File

SOURCE=vscreoj.c
# End Source File
# Begin Source File

SOURCE=vscrffld.c
# End Source File
# Begin Source File

SOURCE=vscrfile.c
# End Source File
# Begin Source File

SOURCE=vscrglb.c
# End Source File
# Begin Source File

SOURCE=vscrhelp.c
# End Source File
# Begin Source File

SOURCE=vscrkwfo.c
# End Source File
# Begin Source File

SOURCE=vscrmain.c
# End Source File
# Begin Source File

SOURCE=vscrout.c
# End Source File
# Begin Source File

SOURCE=vscrspky.c
# End Source File
# Begin Source File

SOURCE=vscrsqz.c
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl;fi;fd"
# Begin Source File

SOURCE=gp.h
# End Source File
# Begin Source File

SOURCE=ll.h
# End Source File
# Begin Source File

SOURCE=vscrglb.h
# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;cnt;rtf;gif;jpg;jpeg;jpe"
# End Group
# End Target
# End Project
