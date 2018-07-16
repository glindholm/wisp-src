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

# Begin Group "SHIP"

# PROP Default_Filter ""
# Begin Group "CONFIG"

# PROP Default_Filter ""
# Begin Group "videocap"

# PROP Default_Filter "vcap"
# Begin Source File

SOURCE=..\videocap\aix_xterm.vcap
# End Source File
# Begin Source File

SOURCE=..\videocap\aixterm.vcap
# End Source File
# Begin Source File

SOURCE=..\videocap\ansi.vcap
# End Source File
# Begin Source File

SOURCE=..\videocap\ansidos.vcap
# End Source File
# Begin Source File

SOURCE=..\videocap\att605.vcap
# End Source File
# Begin Source File

SOURCE=..\videocap\decterm.vcap
# End Source File
# Begin Source File

SOURCE=..\videocap\dg_xterm.vcap
# End Source File
# Begin Source File

SOURCE=..\videocap\ibm3151.vcap
# End Source File
# Begin Source File

SOURCE=..\videocap\ibm8514.vcap
# End Source File
# Begin Source File

SOURCE=..\videocap\ipx_sun.vcap
# End Source File
# Begin Source File

SOURCE=..\videocap\ipx_xterm.vcap
# End Source File
# Begin Source File

SOURCE=..\videocap\msdos.vcap
# End Source File
# Begin Source File

SOURCE=..\videocap\qvt101p.vcap
# End Source File
# Begin Source File

SOURCE=..\videocap\rflvt220.vcap
# End Source File
# Begin Source File

SOURCE=..\videocap\sco_xterm.vcap
# End Source File
# Begin Source File

SOURCE=..\videocap\stddef.vcap
# End Source File
# Begin Source File

SOURCE=..\videocap\sun_cmd.vcap
# End Source File
# Begin Source File

SOURCE=..\videocap\uterm.vcap
# End Source File
# Begin Source File

SOURCE=..\videocap\uw_xterm.vcap
# End Source File
# Begin Source File

SOURCE=..\videocap\vt100.vcap
# End Source File
# Begin Source File

SOURCE=..\videocap\vt220.vcap
# End Source File
# Begin Source File

SOURCE=..\videocap\vt510.vcap
# End Source File
# Begin Source File

SOURCE=..\videocap\w4w_vt220.vcap
# End Source File
# Begin Source File

SOURCE=..\videocap\wincon.vcap
# End Source File
# Begin Source File

SOURCE=..\videocap\wy_vt100.vcap
# End Source File
# Begin Source File

SOURCE=..\videocap\xterm.vcap
# End Source File
# End Group
# Begin Source File

SOURCE=..\nt\ACUCONFIG
# End Source File
# Begin Source File

SOURCE=..\nt\CHARMAP
# End Source File
# Begin Source File

SOURCE=..\nt\CQMAP
# End Source File
# Begin Source File

SOURCE=..\nt\FORMS
# End Source File
# Begin Source File

SOURCE=..\nt\LGMAP
# End Source File
# Begin Source File

SOURCE=..\nt\LPMAP
# End Source File
# Begin Source File

SOURCE=..\etc\OPTIONS
# End Source File
# Begin Source File

SOURCE=..\nt\PRMAP
# End Source File
# Begin Source File

SOURCE=..\etc\W4WMAP
# End Source File
# Begin Source File

SOURCE=..\etc\wispmsg.txt
# End Source File
# Begin Source File

SOURCE=..\wproc\wproc.msg
# End Source File
# Begin Source File

SOURCE=..\nt\wrun.cfg
# End Source File
# Begin Source File

SOURCE=..\nt\wsysconf.cfg
# End Source File
# End Group
# Begin Group "ETC"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\doc\aqmwisp.txt
# End Source File
# Begin Source File

SOURCE=..\wisputils\disprint.mak
# End Source File
# Begin Source File

SOURCE=..\wisputils\disprint.umf
# End Source File
# Begin Source File

SOURCE=..\wisputils\DISPRINT.wcb
# End Source File
# Begin Source File

SOURCE=.\make.include
# End Source File
# Begin Source File

SOURCE=..\wisputils\SOFTLINK.wcb
# End Source File
# Begin Source File

SOURCE=..\doc\wisp_install_unix.txt
# End Source File
# Begin Source File

SOURCE=..\doc\wisp_packlist.txt
# End Source File
# Begin Source File

SOURCE=..\doc\wisp_relnotes.txt
# End Source File
# Begin Source File

SOURCE=..\wisputils\WISPPLAT.wcb
# End Source File
# Begin Source File

SOURCE=..\wisptran\words.def
# End Source File
# Begin Source File

SOURCE=..\doc\wproc.txt
# End Source File
# End Group
# Begin Source File

SOURCE=..\doc\wispntdoc.txt
# End Source File
# Begin Source File

SOURCE=..\doc\wispntsetup.txt
# End Source File
# End Group
# End Target
# End Project
