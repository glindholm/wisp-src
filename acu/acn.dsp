# Microsoft Developer Studio Project File - Name="acn" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) External Target" 0x0106

CFG=acn - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "acn.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "acn.mak" CFG="acn - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "acn - Win32 Release" (based on "Win32 (x86) External Target")
!MESSAGE "acn - Win32 Debug" (based on "Win32 (x86) External Target")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""

!IF  "$(CFG)" == "acn - Win32 Release"

# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Cmd_Line "NMAKE /f acn.mak"
# PROP BASE Rebuild_Opt "/a"
# PROP BASE Target_File "acn.exe"
# PROP BASE Bsc_Name "acn.bsc"
# PROP BASE Target_Dir ""
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Cmd_Line "NMAKE /f wacu.mak WISPDIR=.. acn"
# PROP Rebuild_Opt "/a"
# PROP Target_File "ACN"
# PROP Bsc_Name "acn.bsc"
# PROP Target_Dir ""

!ELSEIF  "$(CFG)" == "acn - Win32 Debug"

# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Cmd_Line "NMAKE /f acn.mak"
# PROP BASE Rebuild_Opt "/a"
# PROP BASE Target_File "acn.exe"
# PROP BASE Bsc_Name "acn.bsc"
# PROP BASE Target_Dir ""
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Cmd_Line "NMAKE /f wacu.mak WISPDIR=.. acn"
# PROP Rebuild_Opt "/a"
# PROP Target_File "ACN"
# PROP Bsc_Name "acn.bsc"
# PROP Target_Dir ""

!ENDIF 

# Begin Target

# Name "acn - Win32 Release"
# Name "acn - Win32 Debug"

!IF  "$(CFG)" == "acn - Win32 Release"

!ELSEIF  "$(CFG)" == "acn - Win32 Debug"

!ENDIF 

# Begin Source File

SOURCE=.\wacu.mak
# End Source File
# Begin Source File

SOURCE=.\WACUDISPLAY.cob
# End Source File
# Begin Source File

SOURCE=.\WACUERROR.cob
# End Source File
# Begin Source File

SOURCE=.\WACUFAC2SCREEN.cob
# End Source File
# Begin Source File

SOURCE=.\WACUGETPARM.cob
# End Source File
# Begin Source File

SOURCE=.\WACUGETPFKEY.cob
# End Source File
# Begin Source File

SOURCE=.\WACUHELP.cob
# End Source File
# Begin Source File

SOURCE=.\WACUWSB.cob
# End Source File
# End Target
# End Project
