# Microsoft Developer Studio Project File - Name="wisplib" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=wisplib - Win32 Release
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "wisplib.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "wisplib.mak" CFG="wisplib - Win32 Release"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "wisplib - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "wisplib - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "wisplib - Win32 Release"

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
# ADD CPP /nologo /MD /W3 /GX /O2 /I "..\wispcommon" /I "..\videolib" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "WINNT" /D "MSFS" /FR /YX /FD /c
# ADD BASE RSC /l 0x409
# ADD RSC /l 0x409
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo /out:"..\lib\wisp.lib"

!ELSEIF  "$(CFG)" == "wisplib - Win32 Debug"

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
# ADD CPP /nologo /MDd /W3 /GX /Z7 /Od /I "..\wispcommon" /I "..\videolib" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "WINNT" /D "MSFS" /FR /YX /FD /c
# ADD BASE RSC /l 0x409
# ADD RSC /l 0x409
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo /out:"..\lib\wispd.lib"

!ENDIF 

# Begin Target

# Name "wisplib - Win32 Release"
# Name "wisplib - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;hpj;bat;for;f90"
# Begin Source File

SOURCE=.\acustubs.c
# End Source File
# Begin Source File

SOURCE=.\backgrnd.c
# End Source File
# Begin Source File

SOURCE=.\bell.c
# End Source File
# Begin Source File

SOURCE=.\bit_x.c
# End Source File
# Begin Source File

SOURCE=.\bits.c
# End Source File
# Begin Source File

SOURCE=.\breakacp.c
# End Source File
# Begin Source File

SOURCE=.\cancel.c
# End Source File
# Begin Source File

SOURCE=.\cexit.c
# End Source File
# Begin Source File

SOURCE=.\checkacp.c
# End Source File
# Begin Source File

SOURCE=.\closeacp.c
# End Source File
# Begin Source File

SOURCE=.\coblink.c
# End Source File
# Begin Source File

SOURCE=.\cobpic.c
# End Source File
# Begin Source File

SOURCE=.\cobscrn.c
# End Source File
# Begin Source File

SOURCE=.\costar.c
# End Source File
# Begin Source File

SOURCE=.\customvw.c
# End Source File
# Begin Source File

SOURCE=.\date.c
# End Source File
# Begin Source File

SOURCE=.\day.c
# End Source File
# Begin Source File

SOURCE=.\dbfile.c
# End Source File
# Begin Source File

SOURCE=.\edestubs.c
# End Source File
# Begin Source File

SOURCE=.\errgparm.c
# End Source File
# Begin Source File

SOURCE=.\extract.c
# End Source File
# Begin Source File

SOURCE=.\fcopy.c
# End Source File
# Begin Source File

SOURCE=.\fexists.c
# End Source File
# Begin Source File

SOURCE=.\filecopy.c
# End Source File
# Begin Source File

SOURCE=.\filesize.c
# End Source File
# Begin Source File

SOURCE=.\filgparm.c
# End Source File
# Begin Source File

SOURCE=.\find.c
# End Source File
# Begin Source File

SOURCE=.\findcase.c
# End Source File
# Begin Source File

SOURCE=.\findexts.c
# End Source File
# Begin Source File

SOURCE=.\fxzone.c
# End Source File
# Begin Source File

SOURCE=..\wispcommon\getopt.c
# End Source File
# Begin Source File

SOURCE=.\getparm.c
# End Source File
# Begin Source File

SOURCE=.\gparmbld.c
# End Source File
# Begin Source File

SOURCE=.\hexunpk.c
# End Source File
# Begin Source File

SOURCE=.\idsisubs.c
# End Source File
# Begin Source File

SOURCE=.\initglbs.c
# End Source File
# Begin Source File

SOURCE=.\initwisp.c
# End Source File
# Begin Source File

SOURCE=.\isexec.c
# End Source File
# Begin Source File

SOURCE=.\level.c
# End Source File
# Begin Source File

SOURCE=.\licwisp.c
# End Source File
# Begin Source File

SOURCE=.\link.c
# End Source File
# Begin Source File

SOURCE=.\linkproc.c
# End Source File
# Begin Source File

SOURCE=.\linksubs.c
# End Source File
# Begin Source File

SOURCE=.\linkvect.c
# End Source File
# Begin Source File

SOURCE=.\logoff.c
# End Source File
# Begin Source File

SOURCE=.\longarg.c
# End Source File
# Begin Source File

SOURCE=.\machid.c
# End Source File
# Begin Source File

SOURCE=.\makepath.c
# End Source File
# Begin Source File

SOURCE=.\menu_go.c
# End Source File
# Begin Source File

SOURCE=.\menu_key.c
# End Source File
# Begin Source File

SOURCE=.\menuread.c
# End Source File
# Begin Source File

SOURCE=.\menuscan.c
# End Source File
# Begin Source File

SOURCE=.\message.c
# End Source File
# Begin Source File

SOURCE=.\mfstubs.c
# End Source File
# Begin Source File

SOURCE=.\mngfile.c
# End Source File
# Begin Source File

SOURCE=.\mwconv.c
# End Source File
# Begin Source File

SOURCE=.\nextfile.c
# End Source File
# Begin Source File

SOURCE=.\onhelp.c
# End Source File
# Begin Source File

SOURCE=.\openacp.c
# End Source File
# Begin Source File

SOURCE=.\operator.c
# End Source File
# Begin Source File

SOURCE=.\packer.c
# End Source File
# Begin Source File

SOURCE=.\paths.c
# End Source File
# Begin Source File

SOURCE=.\platsubs.c
# End Source File
# Begin Source File

SOURCE=.\print.c
# End Source File
# Begin Source File

SOURCE=.\putparm.c
# End Source File
# Begin Source File

SOURCE=.\pwdname.c
# End Source File
# Begin Source File

SOURCE=.\readacp.c
# End Source File
# Begin Source File

SOURCE=.\readfdr.c
# End Source File
# Begin Source File

SOURCE=.\readvtoc.c
# End Source File
# Begin Source File

SOURCE=.\rename.c
# End Source File
# Begin Source File

SOURCE=.\retcode.c
# End Source File
# Begin Source File

SOURCE=..\wispcommon\ring.c
# End Source File
# Begin Source File

SOURCE=.\runtype.c
# End Source File
# Begin Source File

SOURCE=.\rvmap.c
# End Source File
# Begin Source File

SOURCE=.\scratch.c
# End Source File
# Begin Source File

SOURCE=.\screen.c
# End Source File
# Begin Source File

SOURCE=.\search.c
# End Source File
# Begin Source File

SOURCE=.\set.c
# End Source File
# Begin Source File

SOURCE=.\setenvst.c
# End Source File
# Begin Source File

SOURCE=.\setfile.c
# End Source File
# Begin Source File

SOURCE=.\setprgid.c
# End Source File
# Begin Source File

SOURCE=.\settrigp.c
# End Source File
# Begin Source File

SOURCE=.\sharemem.c
# End Source File
# Begin Source File

SOURCE=.\shutexit.c
# End Source File
# Begin Source File

SOURCE=.\sort.c
# End Source File
# Begin Source File

SOURCE=.\sortcall.c
# End Source File
# Begin Source File

SOURCE=.\sortlink.c
# End Source File
# Begin Source File

SOURCE=.\sortseqf.c
# End Source File
# Begin Source File

SOURCE=.\string.c
# End Source File
# Begin Source File

SOURCE=.\submit.c
# End Source File
# Begin Source File

SOURCE=.\untabify.c
# End Source File
# Begin Source File

SOURCE=.\updatfdr.c
# End Source File
# Begin Source File

SOURCE=.\upper.c
# End Source File
# Begin Source File

SOURCE=.\vdisplay.c
# End Source File
# Begin Source File

SOURCE=.\vssort.c
# End Source File
# Begin Source File

SOURCE=.\vwang.c
# End Source File
# Begin Source File

SOURCE=.\waccept.c
# End Source File
# Begin Source File

SOURCE=.\wangmenu.c
# End Source File
# Begin Source File

SOURCE=.\wanguid.c
# End Source File
# Begin Source File

SOURCE=.\wassert.c
# End Source File
# Begin Source File

SOURCE=.\wauthsub.c
# End Source File
# Begin Source File

SOURCE=.\wchain.c
# End Source File
# Begin Source File

SOURCE=.\wcmatch.c
# End Source File
# Begin Source File

SOURCE=.\wdisplay.c
# End Source File
# Begin Source File

SOURCE=.\werrlog.c
# End Source File
# Begin Source File

SOURCE=.\werrpath.c
# End Source File
# Begin Source File

SOURCE=.\werrvre.c
# End Source File
# Begin Source File

SOURCE=.\wexit.c
# End Source File
# Begin Source File

SOURCE=.\wexith.c
# End Source File
# Begin Source File

SOURCE=.\wfaccess.c
# End Source File
# Begin Source File

SOURCE=.\wfcisam.c
# End Source File
# Begin Source File

SOURCE=.\wfclose.c
# End Source File
# Begin Source File

SOURCE=.\wfilechk.c
# End Source File
# Begin Source File

SOURCE=.\wfiledis.c
# End Source File
# Begin Source File

SOURCE=.\wfileext.c
# End Source File
# Begin Source File

SOURCE=.\wfname.c
# End Source File
# Begin Source File

SOURCE=.\wfopen.c
# End Source File
# Begin Source File

SOURCE=.\wftok.c
# End Source File
# Begin Source File

SOURCE=.\wfvision.c
# End Source File
# Begin Source File

SOURCE=.\wfwait.c
# End Source File
# Begin Source File

SOURCE=.\wgetpgrp.c
# End Source File
# Begin Source File

SOURCE=.\wglobals.c
# End Source File
# Begin Source File

SOURCE=.\win32err.c
# End Source File
# Begin Source File

SOURCE=.\win32msg.c
# End Source File
# Begin Source File

SOURCE=.\win32prt.c
# End Source File
# Begin Source File

SOURCE=.\win32spn.c
# End Source File
# Begin Source File

SOURCE=.\winnt.c
# End Source File
# Begin Source File

SOURCE=..\wispcommon\wisp_pic.c
# End Source File
# Begin Source File

SOURCE=.\wispcfg.c
# End Source File
# Begin Source File

SOURCE=.\wispexit.c
# End Source File
# Begin Source File

SOURCE=.\wispsort.c
# End Source File
# Begin Source File

SOURCE=.\wispsync.c
# End Source File
# Begin Source File

SOURCE=.\wispvers.c
# End Source File
# Begin Source File

SOURCE=.\wlickey.c
# End Source File
# Begin Source File

SOURCE=.\wmalloc.c
# End Source File
# Begin Source File

SOURCE=.\wpause.c
# End Source File
# Begin Source File

SOURCE=.\wperson.c
# End Source File
# Begin Source File

SOURCE=.\wprint.c
# End Source File
# Begin Source File

SOURCE=.\writeacp.c
# End Source File
# Begin Source File

SOURCE=.\wrunconf.c
# End Source File
# Begin Source File

SOURCE=.\wsb.c
# End Source File
# Begin Source File

SOURCE=.\wsclose.c
# End Source File
# Begin Source File

SOURCE=.\wscreen.c
# End Source File
# Begin Source File

SOURCE=.\wsfnm.c
# End Source File
# Begin Source File

SOURCE=.\wsfns.c
# End Source File
# Begin Source File

SOURCE=.\wshelp.c
# End Source File
# Begin Source File

SOURCE=.\wswap.c
# End Source File
# Begin Source File

SOURCE=.\wsxio.c
# End Source File
# Begin Source File

SOURCE=.\wsystem.c
# End Source File
# Begin Source File

SOURCE=.\wvaset.c
# End Source File
# Begin Source File

SOURCE=.\wwaitpid.c
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl;fi;fd"
# Begin Source File

SOURCE=.\acp.h
# End Source File
# Begin Source File

SOURCE=.\assert.h
# End Source File
# Begin Source File

SOURCE=.\cobpic.h
# End Source File
# Begin Source File

SOURCE=.\cobrun.h
# End Source File
# Begin Source File

SOURCE=.\cobscrn.h
# End Source File
# Begin Source File

SOURCE=..\wispcommon\costar.h
# End Source File
# Begin Source File

SOURCE=.\day.h
# End Source File
# Begin Source File

SOURCE=.\fcopy.h
# End Source File
# Begin Source File

SOURCE=..\wispcommon\filext.h
# End Source File
# Begin Source File

SOURCE=.\filgparm.h
# End Source File
# Begin Source File

SOURCE=..\wispcommon\getopt.h
# End Source File
# Begin Source File

SOURCE=..\wispcommon\hexunpk.h
# End Source File
# Begin Source File

SOURCE=..\wispcommon\idsistd.h
# End Source File
# Begin Source File

SOURCE=..\wispcommon\idsisubs.h
# End Source File
# Begin Source File

SOURCE=..\wispcommon\intdef.h
# End Source File
# Begin Source File

SOURCE=..\wispcommon\level.h
# End Source File
# Begin Source File

SOURCE=..\wispcommon\link.h
# End Source File
# Begin Source File

SOURCE=.\linkvect.h
# End Source File
# Begin Source File

SOURCE=..\wispcommon\machid.h
# End Source File
# Begin Source File

SOURCE=.\menu.h
# End Source File
# Begin Source File

SOURCE=.\osddefs.h
# End Source File
# Begin Source File

SOURCE=.\p0x386.h
# End Source File
# Begin Source File

SOURCE=.\p2x386.h
# End Source File
# Begin Source File

SOURCE=..\wispcommon\paths.h
# End Source File
# Begin Source File

SOURCE=..\wispcommon\platsubs.h
# End Source File
# Begin Source File

SOURCE=..\wispcommon\prompt.h
# End Source File
# Begin Source File

SOURCE=..\wispcommon\putparm.h
# End Source File
# Begin Source File

SOURCE=..\wispcommon\ring.h
# End Source File
# Begin Source File

SOURCE=.\runtype.h
# End Source File
# Begin Source File

SOURCE=.\rvmap.h
# End Source File
# Begin Source File

SOURCE=..\wispcommon\scnfacs.h
# End Source File
# Begin Source File

SOURCE=.\screen.h
# End Source File
# Begin Source File

SOURCE=..\wispcommon\setenvst.h
# End Source File
# Begin Source File

SOURCE=.\setprgid.h
# End Source File
# Begin Source File

SOURCE=..\wispcommon\sharemem.h
# End Source File
# Begin Source File

SOURCE=..\wispcommon\sortseqf.h
# End Source File
# Begin Source File

SOURCE=.\submit.h
# End Source File
# Begin Source File

SOURCE=.\vdispidx.h
# End Source File
# Begin Source File

SOURCE=.\visint.h
# End Source File
# Begin Source File

SOURCE=.\visn2.h
# End Source File
# Begin Source File

SOURCE=.\visn3.h
# End Source File
# Begin Source File

SOURCE=..\wispcommon\vssort.h
# End Source File
# Begin Source File

SOURCE=..\wispcommon\vwang.h
# End Source File
# Begin Source File

SOURCE=..\wispcommon\wangkeys.h
# End Source File
# Begin Source File

SOURCE=..\wispcommon\wanguid.h
# End Source File
# Begin Source File

SOURCE=..\wispcommon\wcommon.h
# End Source File
# Begin Source File

SOURCE=..\wispcommon\wdefines.h
# End Source File
# Begin Source File

SOURCE=..\wispcommon\werrlog.h
# End Source File
# Begin Source File

SOURCE=..\wispcommon\wexit.h
# End Source File
# Begin Source File

SOURCE=.\wfaccess.h
# End Source File
# Begin Source File

SOURCE=..\wispcommon\wfiledis.h
# End Source File
# Begin Source File

SOURCE=..\wispcommon\wfiles.h
# End Source File
# Begin Source File

SOURCE=..\wispcommon\wfname.h
# End Source File
# Begin Source File

SOURCE=..\wispcommon\wglobals.h
# End Source File
# Begin Source File

SOURCE=.\win32err.h
# End Source File
# Begin Source File

SOURCE=.\win32msg.h
# End Source File
# Begin Source File

SOURCE=.\win32spn.h
# End Source File
# Begin Source File

SOURCE=..\wispcommon\win32std.h
# End Source File
# Begin Source File

SOURCE=..\wispcommon\wisp_pic.h
# End Source File
# Begin Source File

SOURCE=..\wispcommon\wispcfg.h
# End Source File
# Begin Source File

SOURCE=..\wispcommon\wisplib.h
# End Source File
# Begin Source File

SOURCE=..\wispcommon\wispnt.h
# End Source File
# Begin Source File

SOURCE=..\wispcommon\wispvers.h
# End Source File
# Begin Source File

SOURCE=..\wispcommon\wlicense.h
# End Source File
# Begin Source File

SOURCE=..\wispcommon\wmalloc.h
# End Source File
# Begin Source File

SOURCE=..\wispcommon\wperson.h
# End Source File
# Begin Source File

SOURCE=.\wplatdef.h
# End Source File
# Begin Source File

SOURCE=..\wispcommon\wrunconf.h
# End Source File
# Begin Source File

SOURCE=.\wsb.h
# End Source File
# Begin Source File

SOURCE=.\wsfns.h
# End Source File
# Begin Source File

SOURCE=..\wispcommon\wsysconf.h
# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;cnt;rtf;gif;jpg;jpeg;jpe"
# End Group
# Begin Source File

SOURCE=.\libwisp.umf
# End Source File
# End Target
# End Project
