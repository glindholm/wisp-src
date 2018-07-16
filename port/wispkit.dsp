# Microsoft Developer Studio Project File - Name="wispkit" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) External Target" 0x0106

CFG=wispkit - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "wispkit.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "wispkit.mak" CFG="wispkit - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "wispkit - Win32 Release" (based on "Win32 (x86) External Target")
!MESSAGE "wispkit - Win32 Debug" (based on "Win32 (x86) External Target")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""

!IF  "$(CFG)" == "wispkit - Win32 Release"

# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Cmd_Line "NMAKE /f wispkit.mak"
# PROP BASE Rebuild_Opt "/a"
# PROP BASE Target_File "wispkit.exe"
# PROP BASE Bsc_Name "wispkit.bsc"
# PROP BASE Target_Dir ""
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Cmd_Line "ECHO WISPKIT IS BUILT"
# PROP Rebuild_Opt "/a"
# PROP Target_File "WISPKIT"
# PROP Bsc_Name "wispkit.bsc"
# PROP Target_Dir ""

!ELSEIF  "$(CFG)" == "wispkit - Win32 Debug"

# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Cmd_Line "NMAKE /f wispkit.mak"
# PROP BASE Rebuild_Opt "/a"
# PROP BASE Target_File "wispkit.exe"
# PROP BASE Bsc_Name "wispkit.bsc"
# PROP BASE Target_Dir ""
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Cmd_Line "ECHO WISPKIT IS BUILT"
# PROP Rebuild_Opt "/a"
# PROP Target_File "WISPKIT"
# PROP Bsc_Name "wispkit.bsc"
# PROP Target_Dir ""

!ENDIF 

# Begin Target

# Name "wispkit - Win32 Release"
# Name "wispkit - Win32 Debug"

!IF  "$(CFG)" == "wispkit - Win32 Release"

!ELSEIF  "$(CFG)" == "wispkit - Win32 Debug"

!ENDIF 

# Begin Group "DOC"

# PROP Default_Filter "txt;lis"
# Begin Source File

SOURCE=..\etc\aqmwisp.txt
# End Source File
# Begin Source File

SOURCE=..\etc\nonascii.txt
# End Source File
# Begin Source File

SOURCE=..\etc\nttelnet.txt
# End Source File
# Begin Source File

SOURCE=..\etc\vcolors.txt
# End Source File
# Begin Source File

SOURCE=..\etc\wisp_install_unix.txt
# End Source File
# Begin Source File

SOURCE=..\etc\wisp_relnotes.txt
# End Source File
# Begin Source File

SOURCE=..\etc\wispacn.txt
# End Source File
# Begin Source File

SOURCE=..\etc\wispmsg.txt
# End Source File
# Begin Source File

SOURCE=..\etc\wispntdoc.txt
# End Source File
# Begin Source File

SOURCE=..\etc\wispntsetup.txt
# End Source File
# End Group
# End Target
# End Project
