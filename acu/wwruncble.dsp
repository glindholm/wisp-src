# Microsoft Developer Studio Project File - Name="wwruncble" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) External Target" 0x0106

CFG=wwruncble - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "wwruncble.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "wwruncble.mak" CFG="wwruncble - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "wwruncble - Win32 Release" (based on "Win32 (x86) External Target")
!MESSAGE "wwruncble - Win32 Debug" (based on "Win32 (x86) External Target")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""

!IF  "$(CFG)" == "wwruncble - Win32 Release"

# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Cmd_Line "NMAKE /f wwruncble.mak"
# PROP BASE Rebuild_Opt "/a"
# PROP BASE Target_File "wwruncble.exe"
# PROP BASE Bsc_Name "wwruncble.bsc"
# PROP BASE Target_Dir ""
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Cmd_Line "NMAKE /f wwruncbl.mak WISPDIR=.. EDEDIR=..\ede ede"
# PROP Rebuild_Opt "/a"
# PROP Target_File "wwruncble.exe"
# PROP Bsc_Name "wwruncble.bsc"
# PROP Target_Dir ""

!ELSEIF  "$(CFG)" == "wwruncble - Win32 Debug"

# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Cmd_Line "NMAKE /f wwruncble.mak"
# PROP BASE Rebuild_Opt "/a"
# PROP BASE Target_File "wwruncble.exe"
# PROP BASE Bsc_Name "wwruncble.bsc"
# PROP BASE Target_Dir ""
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Cmd_Line "NMAKE /f wwruncbl.mak L_WISP=wispd L_VIDEO=videod L_EDE=eded WISPDIR=.. EDEDIR=..\ede EXTRA_LINK=msvcrtd.lib ede"
# PROP Rebuild_Opt "/a"
# PROP Target_File "wwruncble.exe"
# PROP Bsc_Name "wwruncble.bsc"
# PROP Target_Dir ""

!ENDIF 

# Begin Target

# Name "wwruncble - Win32 Release"
# Name "wwruncble - Win32 Debug"

!IF  "$(CFG)" == "wwruncble - Win32 Release"

!ELSEIF  "$(CFG)" == "wwruncble - Win32 Debug"

!ENDIF 

# Begin Source File

SOURCE=.\sub85.c
# End Source File
# Begin Source File

SOURCE=.\wwruncbl.mak
# End Source File
# End Target
# End Project
