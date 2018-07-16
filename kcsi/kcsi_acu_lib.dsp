# Microsoft Developer Studio Project File - Name="kcsi_acu_lib" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=kcsi_acu_lib - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "kcsi_acu_lib.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "kcsi_acu_lib.mak" CFG="kcsi_acu_lib - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "kcsi_acu_lib - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "kcsi_acu_lib - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "kcsi_acu_lib - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "kcsi_acu_lib___Win32_Release"
# PROP BASE Intermediate_Dir "kcsi_acu_lib___Win32_Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "kcsi_acu_lib___Win32_Release"
# PROP Intermediate_Dir "kcsi_acu_lib___Win32_Release"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /YX /FD /c
# ADD CPP /nologo /MD /W3 /GX /O2 /I "..\wispcommon" /D "NDEBUG" /D "WIN32" /D "_MBCS" /D "_LIB" /D "KCSI_WIN32" /D "KCSI_ACU" /FR /YX /FD /c
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo /out:"kcsiacu.lib"

!ELSEIF  "$(CFG)" == "kcsi_acu_lib - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "kcsi_acu_lib___Win32_Debug"
# PROP BASE Intermediate_Dir "kcsi_acu_lib___Win32_Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "kcsi_acu_lib___Win32_Debug"
# PROP Intermediate_Dir "kcsi_acu_lib___Win32_Debug"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /YX /FD /GZ /c
# ADD CPP /nologo /MDd /W3 /Gm /GX /ZI /Od /I "..\wispcommon" /D "_DEBUG" /D "WIN32" /D "_MBCS" /D "_LIB" /D "KCSI_WIN32" /D "KCSI_ACU" /FR /YX /FD /GZ /c
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo /out:"kcsiacu_d.lib"

!ENDIF 

# Begin Target

# Name "kcsi_acu_lib - Win32 Release"
# Name "kcsi_acu_lib - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=.\brlio.c
# End Source File
# Begin Source File

SOURCE=.\bub.c
# End Source File
# Begin Source File

SOURCE=.\ccsioerr.c
# End Source File
# Begin Source File

SOURCE=.\cridebug.c
# End Source File
# Begin Source File

SOURCE=.\cridvers.c
# End Source File
# Begin Source File

SOURCE=.\dadd.c
# End Source File
# Begin Source File

SOURCE=.\daux.c
# End Source File
# Begin Source File

SOURCE=.\dbsc.c
# End Source File
# Begin Source File

SOURCE=.\dchg.c
# End Source File
# Begin Source File

SOURCE=.\ddel.c
# End Source File
# Begin Source File

SOURCE=.\dglb.c
# End Source File
# Begin Source File

SOURCE=.\dkey.c
# End Source File
# Begin Source File

SOURCE=.\dmnt.c
# End Source File
# Begin Source File

SOURCE=.\dtedat.c
# End Source File
# Begin Source File

SOURCE=.\dtekey.c
# End Source File
# Begin Source File

SOURCE=.\dval.c
# End Source File
# Begin Source File

SOURCE=.\gp.c
# End Source File
# Begin Source File

SOURCE=.\igen.c
# End Source File
# Begin Source File

SOURCE=.\iglb.c
# End Source File
# Begin Source File

SOURCE=.\inidio.c
# End Source File
# Begin Source File

SOURCE=.\iprs.c
# End Source File
# Begin Source File

SOURCE=.\itkn.c
# End Source File
# Begin Source File

SOURCE=.\iwrt.c
# End Source File
# Begin Source File

SOURCE=.\kcsio.c
# End Source File
# Begin Source File

SOURCE=.\kcsit.c
# End Source File
# Begin Source File

SOURCE=.\kdisp.c
# End Source File
# Begin Source File

SOURCE=.\kexists.c
# End Source File
# Begin Source File

SOURCE=.\kv3.c
# End Source File
# Begin Source File

SOURCE=.\ll.c
# End Source File
# Begin Source File

SOURCE=.\piclen.c
# End Source File
# Begin Source File

SOURCE=.\rbld.c
# End Source File
# Begin Source File

SOURCE=.\rcal.c
# End Source File
# Begin Source File

SOURCE=.\rcmp.c
# End Source File
# Begin Source File

SOURCE=.\rcvs.c
# End Source File
# Begin Source File

SOURCE=.\rcvt.c
# End Source File
# Begin Source File

SOURCE=.\rfmt.c
# End Source File
# Begin Source File

SOURCE=.\rglb.c
# End Source File
# Begin Source File

SOURCE=.\rpln.c
# End Source File
# Begin Source File

SOURCE=.\rsel.c
# End Source File
# Begin Source File

SOURCE=.\rsrt.c
# End Source File
# Begin Source File

SOURCE=.\rtie.c
# End Source File
# Begin Source File

SOURCE=.\rwhlp.c
# End Source File
# Begin Source File

SOURCE=.\rwop.c
# End Source File
# Begin Source File

SOURCE=.\rwrt.c
# End Source File
# Begin Source File

SOURCE=.\rwsrt.c
# End Source File
# Begin Source File

SOURCE=.\seqio.c
# End Source File
# Begin Source File

SOURCE=.\strtkn.c
# End Source File
# Begin Source File

SOURCE=.\valflv.c
# End Source File
# Begin Source File

SOURCE=.\vcsio.c
# End Source File
# Begin Source File

SOURCE=.\version.c
# End Source File
# Begin Source File

SOURCE=.\vscrbchk.c
# End Source File
# Begin Source File

SOURCE=.\vscrblk.c
# End Source File
# Begin Source File

SOURCE=.\vscrdbg.c
# End Source File
# Begin Source File

SOURCE=.\vscreoj.c
# End Source File
# Begin Source File

SOURCE=.\vscrffld.c
# End Source File
# Begin Source File

SOURCE=.\vscrfile.c
# End Source File
# Begin Source File

SOURCE=.\vscrglb.c
# End Source File
# Begin Source File

SOURCE=.\vscrhelp.c
# End Source File
# Begin Source File

SOURCE=.\vscrout.c
# End Source File
# Begin Source File

SOURCE=.\vscrspky.c
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Source File

SOURCE=.\assert.h
# End Source File
# Begin Source File

SOURCE=.\cobioblk.h
# End Source File
# Begin Source File

SOURCE=.\cobstat.h
# End Source File
# Begin Source File

SOURCE=.\create.h
# End Source File
# Begin Source File

SOURCE=.\crec.h
# End Source File
# Begin Source File

SOURCE=.\crid.h
# End Source File
# Begin Source File

SOURCE=.\cridebug.h
# End Source File
# Begin Source File

SOURCE=.\datcob.h
# End Source File
# Begin Source File

SOURCE=.\dbsc.h
# End Source File
# Begin Source File

SOURCE=.\dcontrol.h
# End Source File
# Begin Source File

SOURCE=.\dglb.h
# End Source File
# Begin Source File

SOURCE=.\disam.h
# End Source File
# Begin Source File

SOURCE=.\dmnt.h
# End Source File
# Begin Source File

SOURCE=.\dtype.h
# End Source File
# Begin Source File

SOURCE=.\dtype2.h
# End Source File
# Begin Source File

SOURCE=.\gp.h
# End Source File
# Begin Source File

SOURCE=.\iglb.h
# End Source File
# Begin Source File

SOURCE=.\intdef.h
# End Source File
# Begin Source File

SOURCE=.\iocode.h
# End Source File
# Begin Source File

SOURCE=.\itkn.h
# End Source File
# Begin Source File

SOURCE=.\kcsifunc.h
# End Source File
# Begin Source File

SOURCE=.\kcsimem.h
# End Source File
# Begin Source File

SOURCE=.\kcsio.h
# End Source File
# Begin Source File

SOURCE=.\kcsit.h
# End Source File
# Begin Source File

SOURCE=.\kisam.h
# End Source File
# Begin Source File

SOURCE=.\kwisp.h
# End Source File
# Begin Source File

SOURCE=.\ll.h
# End Source File
# Begin Source File

SOURCE=.\mffcd.h
# End Source File
# Begin Source File

SOURCE=.\mffcd01.h
# End Source File
# Begin Source File

SOURCE=.\rlmg.h
# End Source File
# Begin Source File

SOURCE=.\rptcob.h
# End Source File
# Begin Source File

SOURCE=.\rptglb.h
# End Source File
# Begin Source File

SOURCE=.\rptprm.h
# End Source File
# Begin Source File

SOURCE=.\rptsrt.h
# End Source File
# Begin Source File

SOURCE=.\shrthand.h
# End Source File
# Begin Source File

SOURCE=.\strtkn.h
# End Source File
# Begin Source File

SOURCE=.\visn2.h
# End Source File
# Begin Source File

SOURCE=.\visn3.h
# End Source File
# Begin Source File

SOURCE=.\vscrglb.h
# End Source File
# Begin Source File

SOURCE=.\wispscr.h
# End Source File
# End Group
# End Target
# End Project
