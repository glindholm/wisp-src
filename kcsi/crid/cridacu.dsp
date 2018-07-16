# Microsoft Developer Studio Project File - Name="cridacu" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) External Target" 0x0106

CFG=cridacu - Win32 Release
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "cridacu.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "cridacu.mak" CFG="cridacu - Win32 Release"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "cridacu - Win32 Release" (based on "Win32 (x86) External Target")
!MESSAGE "cridacu - Win32 Debug" (based on "Win32 (x86) External Target")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""

!IF  "$(CFG)" == "cridacu - Win32 Release"

# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir ".\Release"
# PROP BASE Intermediate_Dir ".\Release"
# PROP BASE Cmd_Line "NMAKE /f cridacu.mak"
# PROP BASE Rebuild_Opt "/a"
# PROP BASE Target_File "cridacu\cridacu.exe"
# PROP BASE Bsc_Name "cridacu\cridacu.bsc"
# PROP BASE Target_Dir "."
# PROP Use_Debug_Libraries 0
# PROP Output_Dir ".\Release"
# PROP Intermediate_Dir ".\Release"
# PROP Cmd_Line "ECHO CRIDACU IS BUILT"
# PROP Rebuild_Opt "/a"
# PROP Target_File "CRIDACU"
# PROP Bsc_Name ""
# PROP Target_Dir "."

!ELSEIF  "$(CFG)" == "cridacu - Win32 Debug"

# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir ".\Debug"
# PROP BASE Intermediate_Dir ".\Debug"
# PROP BASE Cmd_Line "NMAKE /f cridacu.mak"
# PROP BASE Rebuild_Opt "/a"
# PROP BASE Target_File "cridacu\cridacu.exe"
# PROP BASE Bsc_Name "cridacu\cridacu.bsc"
# PROP BASE Target_Dir "."
# PROP Use_Debug_Libraries 1
# PROP Output_Dir ".\Debug"
# PROP Intermediate_Dir ".\Debug"
# PROP Cmd_Line "ECHO CRIDACU IS BUILT"
# PROP Rebuild_Opt "/a"
# PROP Target_File "CRIDACU"
# PROP Bsc_Name ""
# PROP Target_Dir "."

!ENDIF 

# Begin Target

# Name "cridacu - Win32 Release"
# Name "cridacu - Win32 Debug"

!IF  "$(CFG)" == "cridacu - Win32 Release"

!ELSEIF  "$(CFG)" == "cridacu - Win32 Debug"

!ENDIF 

# Begin Group "ACU"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\crid.h
# End Source File
# Begin Source File

SOURCE=.\crid85.c
# End Source File
# Begin Source File

SOURCE=.\cridtbl.c
# End Source File
# Begin Source File

SOURCE=..\..\acu\wrun32wisp_crid.mak
# End Source File
# Begin Source File

SOURCE=..\..\acu\wwruncbl.mak
# End Source File
# End Group
# Begin Group "ETC"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\ctlcnvrt.wcb
# End Source File
# Begin Source File

SOURCE=.\rptcnvrt.wcb
# End Source File
# End Group
# Begin Source File

SOURCE=.\cridntsetup.txt
# End Source File
# Begin Source File

SOURCE=.\packlist.lis
# End Source File
# Begin Source File

SOURCE=.\release.lis
# End Source File
# End Target
# End Project
