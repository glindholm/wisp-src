# Microsoft Developer Studio Project File - Name="createacu" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) External Target" 0x0106

CFG=createacu - Win32 Release
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "createacu.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "createacu.mak" CFG="createacu - Win32 Release"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "createacu - Win32 Release" (based on "Win32 (x86) External Target")
!MESSAGE "createacu - Win32 Debug" (based on "Win32 (x86) External Target")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""

!IF  "$(CFG)" == "createacu - Win32 Release"

# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir ".\Release"
# PROP BASE Intermediate_Dir ".\Release"
# PROP BASE Cmd_Line "NMAKE /f createacu.mak"
# PROP BASE Rebuild_Opt "/a"
# PROP BASE Target_File "createacu\createacu.exe"
# PROP BASE Bsc_Name "createacu\createacu.bsc"
# PROP BASE Target_Dir "."
# PROP Use_Debug_Libraries 0
# PROP Output_Dir ".\Release"
# PROP Intermediate_Dir ".\Release"
# PROP Cmd_Line "NMAKE /f ..\..\acu\wwruncbl.mak WISPDIR=..\.. CREATEDIR=. create"
# PROP Rebuild_Opt "/a"
# PROP Target_File "create.exe"
# PROP Bsc_Name "createacu\createacu.bsc"
# PROP Target_Dir "."

!ELSEIF  "$(CFG)" == "createacu - Win32 Debug"

# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir ".\createac"
# PROP BASE Intermediate_Dir ".\createac"
# PROP BASE Cmd_Line "NMAKE /f createacu.mak"
# PROP BASE Rebuild_Opt "/a"
# PROP BASE Target_File "createacu\createacu.exe"
# PROP BASE Bsc_Name "createacu\createacu.bsc"
# PROP BASE Target_Dir "."
# PROP Use_Debug_Libraries 1
# PROP Output_Dir ".\createac"
# PROP Intermediate_Dir ".\createac"
# PROP Cmd_Line "NMAKE /f ..\..\acu\wwruncbl.mak WISPDIR=..\.. CREATEDIR=. L_WISP=wispd L_VIDEO=videod L_CREATE=createacud EXTRA_LINK=msvcrtd.lib create"
# PROP Rebuild_Opt "/a"
# PROP Target_File "create.exe"
# PROP Bsc_Name "createacu\createacu.bsc"
# PROP Target_Dir "."

!ENDIF 

# Begin Target

# Name "createacu - Win32 Release"
# Name "createacu - Win32 Debug"

!IF  "$(CFG)" == "createacu - Win32 Release"

!ELSEIF  "$(CFG)" == "createacu - Win32 Debug"

!ENDIF 

# Begin Source File

SOURCE=.\vscrmain.c
# End Source File
# Begin Source File

SOURCE=..\..\acu\wwruncbl.mak
# End Source File
# End Target
# End Project
