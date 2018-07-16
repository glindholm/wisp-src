# Microsoft Developer Studio Project File - Name="kcsi_acu" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) External Target" 0x0106

CFG=kcsi_acu - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "kcsi_acu.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "kcsi_acu.mak" CFG="kcsi_acu - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "kcsi_acu - Win32 Release" (based on "Win32 (x86) External Target")
!MESSAGE "kcsi_acu - Win32 Debug" (based on "Win32 (x86) External Target")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""

!IF  "$(CFG)" == "kcsi_acu - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "kcsi_acu___Win32_Release"
# PROP BASE Intermediate_Dir "kcsi_acu___Win32_Release"
# PROP BASE Cmd_Line "NMAKE /f kcsi_acu.mak"
# PROP BASE Rebuild_Opt "/a"
# PROP BASE Target_File "kcsi_acu.exe"
# PROP BASE Bsc_Name "kcsi_acu.bsc"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "kcsi_acu___Win32_Release"
# PROP Intermediate_Dir "kcsi_acu___Win32_Release"
# PROP Cmd_Line "ECHO KCSI_ACU IS BUILT"
# PROP Rebuild_Opt "/a"
# PROP Target_File "KCSI_ACU"
# PROP Bsc_Name "kcsi_acu.bsc"
# PROP Target_Dir ""

!ELSEIF  "$(CFG)" == "kcsi_acu - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "kcsi_acu___Win32_Debug"
# PROP BASE Intermediate_Dir "kcsi_acu___Win32_Debug"
# PROP BASE Cmd_Line "NMAKE /f kcsi_acu.mak"
# PROP BASE Rebuild_Opt "/a"
# PROP BASE Target_File "kcsi_acu.exe"
# PROP BASE Bsc_Name "kcsi_acu.bsc"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "kcsi_acu___Win32_Debug"
# PROP Intermediate_Dir "kcsi_acu___Win32_Debug"
# PROP Cmd_Line "ECHO KCSI_ACU IS BUILT"
# PROP Rebuild_Opt "/a"
# PROP Target_File "KCSI_ACU"
# PROP Bsc_Name ""
# PROP Target_Dir ""

!ENDIF 

# Begin Target

# Name "kcsi_acu - Win32 Release"
# Name "kcsi_acu - Win32 Debug"

!IF  "$(CFG)" == "kcsi_acu - Win32 Release"

!ELSEIF  "$(CFG)" == "kcsi_acu - Win32 Debug"

!ENDIF 

# Begin Group "ACU"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\kcsi_sub85_inc.c
# End Source File
# Begin Source File

SOURCE=..\acu\wrun32wisp_kcsi_acu52.mak
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
# Begin Group "MF"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\control.c
# End Source File
# Begin Source File

SOURCE=.\create.c
# End Source File
# Begin Source File

SOURCE=.\datentry.c
# End Source File
# Begin Source File

SOURCE=.\inquiry.c
# End Source File
# Begin Source File

SOURCE=.\report.c
# End Source File
# End Group
# Begin Source File

SOURCE=.\crid_debug_notes.txt
# End Source File
# Begin Source File

SOURCE=.\kcsi_acu_install.txt
# End Source File
# Begin Source File

SOURCE=.\kcsi_mf.umf
# End Source File
# Begin Source File

SOURCE=.\kcsi_mf_install.txt
# End Source File
# Begin Source File

SOURCE=.\kcsi_packlist.txt
# End Source File
# Begin Source File

SOURCE=.\kcsi_relnotes.txt
# End Source File
# Begin Source File

SOURCE=.\kcsicob.mak
# End Source File
# Begin Source File

SOURCE=.\kcsilibs.umf
# End Source File
# Begin Source File

SOURCE=.\kcsintsetup.txt
# End Source File
# End Target
# End Project
