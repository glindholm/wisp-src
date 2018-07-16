#
#	shipkit.mak
#

COPY=copy

BASEDIR=..

KITDIR=		$(BASEDIR)\kit

WISPKIT=	$(KITDIR)\wisp
EDEKIT=		$(KITDIR)\ede
KCSIACUKIT=	$(KITDIR)\kcsiacu
RTSKIT=		$(KITDIR)\rts
VHEADKIT=	$(KITDIR)\v

wisp_dist_list=	$(WISPKIT)\bin\display.exe	\
		$(WISPKIT)\bin\hexed.exe	\
		$(WISPKIT)\bin\proctran.exe 	\
		$(WISPKIT)\bin\rexec.exe 	\
		$(WISPKIT)\bin\vsedit.exe	\
		$(WISPKIT)\bin\vtest.exe	\
		$(WISPKIT)\bin\wconfig.exe	\
		$(WISPKIT)\bin\wcopy.exe	\
		$(WISPKIT)\bin\wdiag.bat	\
		$(WISPKIT)\bin\wdiag.exe	\
		$(WISPKIT)\bin\wisp.exe		\
		$(WISPKIT)\bin\wisptran.exe	\
		$(WISPKIT)\bin\wlicense.exe	\
		$(WISPKIT)\bin\wproc.exe	\
		$(WISPKIT)\bin\wrun.exe		\
		$(WISPKIT)\bin\wrunt.exe	\
		$(WISPKIT)\bin\wshell.exe	\
		$(WISPKIT)\bin\wsort.exe	\
		$(WISPKIT)\bin\wusage.exe	\
		$(WISPKIT)\lib\video.lib	\
		$(WISPKIT)\lib\wisp.lib		\
		$(WISPKIT)\etc\words.def	\
		$(WISPKIT)\etc\DISPRINT.wcb	\
		$(WISPKIT)\etc\disprint.mak	\
		$(WISPKIT)\etc\WISPPLAT.wcb	\
		$(WISPKIT)\etc\SOFTLINK.wcb	\
		$(WISPKIT)\etc\wproc.txt	\
		$(WISPKIT)\etc\wisp_packlist.txt	\
		$(WISPKIT)\etc\wisp_relnotes.txt	\
		$(WISPKIT)\etc\aqmwisp.txt	\
		$(WISPKIT)\etc\wispntdoc.txt	\
		$(WISPKIT)\config\ACUCONFIG	\
		$(WISPKIT)\config\CHARMAP	\
		$(WISPKIT)\config\CQMAP		\
		$(WISPKIT)\config\FORMS		\
		$(WISPKIT)\config\LGMAP		\
		$(WISPKIT)\config\LPMAP		\
		$(WISPKIT)\config\OPTIONS	\
		$(WISPKIT)\config\PRMAP		\
		$(WISPKIT)\config\W4WMAP	\
		$(WISPKIT)\config\wproc.msg	\
		$(WISPKIT)\config\wrun.cfg	\
		$(WISPKIT)\config\wsysconf.cfg 	\
		$(WISPKIT)\config\videocap\wincon.vcap 	\
		$(WISPKIT)\config\videocap\xterm.vcap 	\
		$(WISPKIT)\config\videocap\ansi.vcap 	\
		$(WISPKIT)\acu\ACULINK.wcb	\
		$(WISPKIT)\acu\ACULINK.acu		\
		$(WISPKIT)\acu\ACUUSING.cob	\
		$(WISPKIT)\acu\ACUUSING.acu		\
		$(WISPKIT)\acu\wacu.mak			\
		$(WISPKIT)\acu\WACUERROR.cob		\
		$(WISPKIT)\acu\WACUDISPLAY.cob		\
		$(WISPKIT)\acu\WACUFAC2SCREEN.cob	\
		$(WISPKIT)\acu\WACUGETPARM.cob		\
		$(WISPKIT)\acu\WACUGETPFKEY.cob		\
		$(WISPKIT)\acu\WACUHELP.cob		\
		$(WISPKIT)\acu\WACUWSB.cob		\
		$(WISPKIT)\acu\WACUERROR.acu		\
		$(WISPKIT)\acu\WACUDISPLAY.acu		\
		$(WISPKIT)\acu\WACUFAC2SCREEN.acu	\
		$(WISPKIT)\acu\WACUGETPARM.acu		\
		$(WISPKIT)\acu\WACUGETPFKEY.acu		\
		$(WISPKIT)\acu\WACUHELP.acu		\
		$(WISPKIT)\acu\WACUWSB.acu		\
		$(WISPKIT)\acu\wisp_sub85_inc.c		\
		$(WISPKIT)\acu\wispicon.ico		\
		$(WISPKIT)\acu\acu50\sub85.c			\
		$(WISPKIT)\acu\acu50\wisprts.rc			\
		$(WISPKIT)\acu\acu50\wrun32wisp_acu50.mak	\
		$(WISPKIT)\acu\acu51\sub85.c			\
		$(WISPKIT)\acu\acu51\wisprts.rc			\
		$(WISPKIT)\acu\acu51\wrun32wisp_acu51.mak	\
		$(WISPKIT)\acu\acu52\sub85.c			\
		$(WISPKIT)\acu\acu52\wisprts.rc			\
		$(WISPKIT)\acu\acu52\wrun32wisp_acu52.mak	\
		$(WISPKIT)\acu\acu52\run32wisp_acu52.mak	\
		$(WISPKIT)\acu\acu60\build_wisp_acu60_rts.txt	\
		$(WISPKIT)\acu\acu60\build_wisp_acu60atm_rts.txt	\
		$(WISPKIT)\acu\acu60\run32.rc			\
		$(WISPKIT)\acu\acu60\run32_wisp_acu60.mak	\
		$(WISPKIT)\acu\acu60\run32_wisp_acu60.dsp	\
		$(WISPKIT)\acu\acu60\sub85.c			\
		$(WISPKIT)\acu\acu60\wrundll.rc			\
		$(WISPKIT)\acu\acu60\wrundll_wisp_acu60.mak	\
		$(WISPKIT)\acu\acu60\wrundll_wisp_acu60.dsp	\
		$(WISPKIT)\acu\acu62\build_wisp_acu62_rts.txt	\
		$(WISPKIT)\acu\acu62\sub85.c			\
		$(WISPKIT)\acu\acu62\wisp.rc			\
		$(WISPKIT)\acu\acu62\wrundll.vcproj		\
		$(WISPKIT)\acu\acu72\build_wisp_acu72_rts.txt	\
		$(WISPKIT)\acu\acu72\sub85.c			\
		$(WISPKIT)\acu\acu72\wisp.rc			\
		$(WISPKIT)\acu\acu72\wrundll.vcproj		\
		$(WISPKIT)\acu\acu72\build_wisp_acu72_atm_rts.txt	\
		$(WISPKIT)\acu\acu72\run32.vcproj		\
		$(WISPKIT)\acu\acu81\build_wisp_acu81_rts.txt	\
		$(WISPKIT)\acu\acu81\build_wisp_acu81_atm_rts.txt	\
		$(WISPKIT)\acu\acu81\run32.vcproj		\
		$(WISPKIT)\acu\acu81\sub85.c			\
		$(WISPKIT)\acu\acu81\wisp.rc			\
		$(WISPKIT)\acu\acu81\wrundll.rc		\
		$(WISPKIT)\acu\acu81\wrundll.vcproj		\
		$(WISPKIT)\demo\cursor.wps	\
		$(WISPKIT)\demo\demo.wps	\
		$(WISPKIT)\demo\dr.wps		\
		$(WISPKIT)\demo\ed.wps		\
		$(WISPKIT)\demo\environ.wps	\
		$(WISPKIT)\demo\putparm.wps	\
		$(WISPKIT)\demo\qabckgrd.wcb	\
		$(WISPKIT)\demo\screen.wps	\
		$(WISPKIT)\demo\test.wps	\
		$(WISPKIT)\demo\video.wps	\
		$(WISPKIT)\wispntsetup.txt

ede_dist_list=	$(EDEKIT)\good.exe		\
		$(EDEKIT)\ede.lib		\
		$(EDEKIT)\edentsetup.txt	\
		$(EDEKIT)\wrun32wisp_ede_acu50.mak	\
		$(EDEKIT)\wrun32wisp_ede_acu51.mak	\
		$(EDEKIT)\wrun32wisp_ede_acu52.mak	\
		$(EDEKIT)\wrundll_ede_acu60.mak		\
		$(EDEKIT)\wrundll_ede_acu60.dsp		\
		$(EDEKIT)\build_ede_acu60_rts.txt	\
		$(EDEKIT)\demo\helpmap.dat	\
		$(EDEKIT)\demo\mcbbld.wcb	\
		$(EDEKIT)\demo\mcbcpy1.wcb	\
		$(EDEKIT)\demo\mcbcpy2.wcb	\
		$(EDEKIT)\demo\mcbcpy3.wcb	\
		$(EDEKIT)\demo\mcbedit.wcb	\
		$(EDEKIT)\demo\menudefs.wcb	\
		$(EDEKIT)\demo\menudemo.wcb	\
		$(EDEKIT)\demo\menudisp.wcb	\
		$(EDEKIT)\demo\menulogo.wcb	\
		$(EDEKIT)\demo\menumcbs.wcb	\
		$(EDEKIT)\demo\menuvect.wcb	\
		$(EDEKIT)\demo\mcbcode.hlp	\
		$(EDEKIT)\demo\mcbcol.hlp	\
		$(EDEKIT)\demo\mcbdisp.hlp	\
		$(EDEKIT)\demo\mcbedit.hlp	\
		$(EDEKIT)\demo\mcbicnt.hlp	\
		$(EDEKIT)\demo\mcbname.hlp	\
		$(EDEKIT)\demo\mcbopts.hlp	\
		$(EDEKIT)\demo\mcbrow.hlp	\
		$(EDEKIT)\demo\mcbtext.hlp	\
		$(EDEKIT)\demo\mcbtype.hlp	\
		$(EDEKIT)\demo\mcbvalue.hlp	\
		$(EDEKIT)\demo\mcbwidth.hlp	\
		$(EDEKIT)\demo\menudemo.opt	\
		$(EDEKIT)\demo\menudemo.mak

kcsiacu_dist_list= \
		$(KCSIACUKIT)\CONTROL		\
		$(KCSIACUKIT)\DATENTRY		\
		$(KCSIACUKIT)\INQUIRY		\
		$(KCSIACUKIT)\REPORT		\
		$(KCSIACUKIT)\CREATE		\
		$(KCSIACUKIT)\kcsiacu.lib	\
		$(KCSIACUKIT)\kcsi_sub85_inc.c	\
		$(KCSIACUKIT)\ctlcnvrt.wcb	\
		$(KCSIACUKIT)\rptcnvrt.wcb	\
		$(KCSIACUKIT)\wrun32wisp_kcsi_acu50.mak	\
		$(KCSIACUKIT)\wrun32wisp_kcsi_acu51.mak	\
		$(KCSIACUKIT)\wrun32wisp_kcsi_acu52.mak	\
		$(KCSIACUKIT)\build_kcsi_acu60_rts.txt	\
		$(KCSIACUKIT)\wrundll_kcsi_acu60.mak	\
		$(KCSIACUKIT)\wrundll_kcsi_acu60.dsp	\
		$(KCSIACUKIT)\build_kcsi_acu62_rts.txt	\
		$(KCSIACUKIT)\acu62\wrundll.vcproj	\
		$(KCSIACUKIT)\build_kcsi_acu72_rts.txt	\
		$(KCSIACUKIT)\acu72\wrundll.vcproj	\
		$(KCSIACUKIT)\kcsi_relnotes.txt	\
		$(KCSIACUKIT)\kcsi_packlist.txt	\
		$(KCSIACUKIT)\kcsintsetup.txt	



#
#	targets
#
default: 	wispshipkit edeshipkit kcsiacushipkit

clean:
	-rmdir /q /s $(KITDIR)

wispshipkit:	header wispkitdirs configdirs $(wisp_dist_list)

edeshipkit:	header edekitdirs $(ede_dist_list)

kcsiacushipkit:	header kcsiacukitdirs $(kcsiacu_dist_list)

rtsshipkit:	rtskitdirs $(rts_dist_list)

vheadshipkit:	$(VHEADKIT)
		copy $(BASEDIR)\videolib\*.h $(VHEADKIT)

header: 
	@echo ">>>> CD        = " 
	@CD
	@echo ">>>>"


#
#	internal targets
#

$(KITDIR):
	mkdir $(KITDIR)

wispkit_dir_list= \
	$(WISPKIT)			\
	$(WISPKIT)\acu			\
	$(WISPKIT)\acu\acu50		\
	$(WISPKIT)\acu\acu51		\
	$(WISPKIT)\acu\acu52		\
	$(WISPKIT)\acu\acu60		\
	$(WISPKIT)\acu\acu62		\
	$(WISPKIT)\acu\acu72		\
	$(WISPKIT)\acu\acu81		\
	$(WISPKIT)\bin			\
	$(WISPKIT)\config		\
	$(WISPKIT)\config\videocap	\
	$(WISPKIT)\demo			\
	$(WISPKIT)\etc			\
	$(WISPKIT)\lib			

wispkitdirs: $(KITDIR) $(wispkit_dir_list)

$(wispkit_dir_list):
	mkdir $@

edekit_dir_list= \
	$(EDEKIT) \
	$(EDEKIT)\demo \
	$(EDEKIT)\acu62 \
	$(EDEKIT)\acu72

edekitdirs: $(KITDIR) $(edekit_dir_list)

$(edekit_dir_list):
	mkdir $@

kcsiacu_dir_list= \
	$(KCSIACUKIT) \
	$(KCSIACUKIT)\acu62 \
	$(KCSIACUKIT)\acu72

kcsiacukitdirs: $(KITDIR) $(kcsiacu_dir_list)

$(kcsiacu_dir_list):
	mkdir $@

config_dir_list = \
	$(BASEDIR)\config \
	$(BASEDIR)\config\videocap

$(config_dir_list):
	mkdir $@

configdirs: $(config_dir_list)

$(RTSKIT):
	mkdir $@

rtskitdirs: $(RTSKIT)

$(VHEADKIT):
	mkdir $@


#
#	WISPKIT components
#
$(WISPKIT)\bin\display.exe:	$(BASEDIR)\bin\$(@F)
	$(COPY) $** $@

$(WISPKIT)\bin\hexed.exe:	$(BASEDIR)\bin\$(@F)
	$(COPY) $** $@

$(WISPKIT)\bin\proctran.exe:	$(BASEDIR)\bin\$(@F)
	$(COPY) $** $@

$(WISPKIT)\bin\rexec.exe:	$(BASEDIR)\bin\$(@F)
	$(COPY) $** $@

$(WISPKIT)\bin\vsedit.exe:	$(BASEDIR)\bin\$(@F)
	$(COPY) $** $@

$(WISPKIT)\bin\vtest.exe:	$(BASEDIR)\bin\$(@F)
	$(COPY) $** $@

$(WISPKIT)\bin\wconfig.exe:	$(BASEDIR)\nt\wconfig\$(@F)
	$(COPY) $** $@

$(WISPKIT)\bin\wcopy.exe:	$(BASEDIR)\bin\$(@F)
	$(COPY) $** $@

$(WISPKIT)\bin\wdiag.bat:	$(BASEDIR)\bin\$(@F)
	$(COPY) $** $@

$(BASEDIR)\bin\wdiag.bat:	$(BASEDIR)\wisputils\$(@F)
	$(COPY) $** $@

$(WISPKIT)\bin\wdiag.exe:	$(BASEDIR)\bin\$(@F)
	$(COPY) $** $@

$(WISPKIT)\bin\wisp.exe:	$(BASEDIR)\bin\$(@F)
	$(COPY) $** $@

$(WISPKIT)\bin\wisptran.exe:	$(BASEDIR)\nt\wisptran\$(@F)
	$(COPY) $** $@

$(WISPKIT)\bin\wlicense.exe:	$(BASEDIR)\bin\$(@F)
	$(COPY) $** $@

$(WISPKIT)\bin\wproc.exe:	$(BASEDIR)\bin\$(@F)
	$(COPY) $** $@

$(WISPKIT)\bin\wrun.exe:	$(BASEDIR)\bin\$(@F)
	$(COPY) $** $@

$(WISPKIT)\bin\wrunt.exe:	$(BASEDIR)\bin\$(@F)
	$(COPY) $** $@

$(WISPKIT)\bin\wshell.exe:	$(BASEDIR)\bin\$(@F)
	$(COPY) $** $@

$(WISPKIT)\bin\wsort.exe:	$(BASEDIR)\bin\$(@F)
	$(COPY) $** $@

$(WISPKIT)\bin\wusage.exe:	$(BASEDIR)\bin\$(@F)
	$(COPY) $** $@

$(WISPKIT)\lib\video.lib:	$(BASEDIR)\lib\$(@F)
	$(COPY) $** $@

$(WISPKIT)\lib\wisp.lib:	$(BASEDIR)\lib\$(@F)
	$(COPY) $** $@

$(WISPKIT)\etc\words.def:	$(BASEDIR)\wisptran\$(@F)
	$(COPY) $** $@

$(WISPKIT)\etc\DISPRINT.wcb:	$(BASEDIR)\wisputils\$(@F)
	$(COPY) $** $@

$(WISPKIT)\etc\disprint.mak:	$(BASEDIR)\wisputils\$(@F)
	$(COPY) $** $@

$(WISPKIT)\etc\WISPPLAT.wcb:	$(BASEDIR)\wisputils\$(@F)
	$(COPY) $** $@

$(WISPKIT)\etc\SOFTLINK.wcb:	$(BASEDIR)\wisputils\$(@F)
	$(COPY) $** $@

$(WISPKIT)\etc\wproc.txt:	$(BASEDIR)\doc\$(@F)
	$(COPY) $** $@

$(WISPKIT)\etc\wisp_packlist.txt:	$(BASEDIR)\doc\$(@F)
	$(COPY) $** $@

$(WISPKIT)\etc\wisp_relnotes.txt:	$(BASEDIR)\doc\$(@F)
	$(COPY) $** $@

$(WISPKIT)\etc\aqmwisp.txt:	$(BASEDIR)\doc\$(@F)
	$(COPY) $** $@

$(WISPKIT)\etc\wispntdoc.txt:	$(BASEDIR)\doc\$(@F)
	$(COPY) $** $@

$(WISPKIT)\config\ACUCONFIG:	$(BASEDIR)\config\$(@F)
	$(COPY) $** $@

$(WISPKIT)\config\CHARMAP:	$(BASEDIR)\config\$(@F)
	$(COPY) $** $@

$(WISPKIT)\config\CQMAP:	$(BASEDIR)\config\$(@F)
	$(COPY) $** $@

$(WISPKIT)\config\FORMS:	$(BASEDIR)\config\$(@F)
	$(COPY) $** $@

$(WISPKIT)\config\LGMAP:	$(BASEDIR)\config\$(@F)
	$(COPY) $** $@

$(WISPKIT)\config\LPMAP:	$(BASEDIR)\config\$(@F)
	$(COPY) $** $@

$(WISPKIT)\config\OPTIONS:	$(BASEDIR)\config\$(@F)
	$(COPY) $** $@

$(WISPKIT)\config\PRMAP:	$(BASEDIR)\config\$(@F)
	$(COPY) $** $@

$(WISPKIT)\config\W4WMAP:	$(BASEDIR)\config\$(@F)
	$(COPY) $** $@

$(WISPKIT)\config\wproc.msg:	$(BASEDIR)\config\$(@F)
	$(COPY) $** $@

$(WISPKIT)\config\wrun.cfg:	$(BASEDIR)\config\$(@F)
	$(COPY) $** $@

$(WISPKIT)\config\wsysconf.cfg:	$(BASEDIR)\config\$(@F)
	$(COPY) $** $@

$(WISPKIT)\config\videocap\wincon.vcap: $(BASEDIR)\config\videocap\$(@F)
	$(COPY) $** $@

$(WISPKIT)\config\videocap\xterm.vcap: $(BASEDIR)\config\videocap\$(@F)
	$(COPY) $** $@

$(WISPKIT)\config\videocap\ansi.vcap: $(BASEDIR)\config\videocap\$(@F)
	$(COPY) $** $@

$(WISPKIT)\acu\ACULINK.wcb:	$(BASEDIR)\acu\$(@F)
	$(COPY) $** $@

$(WISPKIT)\acu\ACULINK.acu:	$(BASEDIR)\acu\$(@F)
	$(COPY) $** $@

$(WISPKIT)\acu\ACUUSING.cob:	$(BASEDIR)\acu\$(@F)
	$(COPY) $** $@

$(WISPKIT)\acu\ACUUSING.acu:	$(BASEDIR)\acu\$(@F)
	$(COPY) $** $@

$(WISPKIT)\acu\wacu.mak:		$(BASEDIR)\acu\$(@F)
	$(COPY) $** $@

$(WISPKIT)\acu\WACUERROR.cob:		$(BASEDIR)\acu\$(@F)
	$(COPY) $** $@

$(WISPKIT)\acu\WACUDISPLAY.cob:		$(BASEDIR)\acu\$(@F)
	$(COPY) $** $@

$(WISPKIT)\acu\WACUFAC2SCREEN.cob:	$(BASEDIR)\acu\$(@F)
	$(COPY) $** $@

$(WISPKIT)\acu\WACUGETPARM.cob:		$(BASEDIR)\acu\$(@F)
	$(COPY) $** $@

$(WISPKIT)\acu\WACUGETPFKEY.cob:	$(BASEDIR)\acu\$(@F)
	$(COPY) $** $@

$(WISPKIT)\acu\WACUHELP.cob:		$(BASEDIR)\acu\$(@F)
	$(COPY) $** $@

$(WISPKIT)\acu\WACUWSB.cob:		$(BASEDIR)\acu\$(@F)
	$(COPY) $** $@

$(WISPKIT)\acu\WACUERROR.acu:		$(BASEDIR)\acu\$(@F)
	$(COPY) $** $@

$(WISPKIT)\acu\WACUDISPLAY.acu:		$(BASEDIR)\acu\$(@F)
	$(COPY) $** $@

$(WISPKIT)\acu\WACUFAC2SCREEN.acu:	$(BASEDIR)\acu\$(@F)
	$(COPY) $** $@

$(WISPKIT)\acu\WACUGETPARM.acu:		$(BASEDIR)\acu\$(@F)
	$(COPY) $** $@

$(WISPKIT)\acu\WACUGETPFKEY.acu:	$(BASEDIR)\acu\$(@F)
	$(COPY) $** $@

$(WISPKIT)\acu\WACUHELP.acu:		$(BASEDIR)\acu\$(@F)
	$(COPY) $** $@

$(WISPKIT)\acu\WACUWSB.acu:		$(BASEDIR)\acu\$(@F)
	$(COPY) $** $@

$(WISPKIT)\acu\wisp_sub85_inc.c:	$(BASEDIR)\acu\$(@F)
	$(COPY) $** $@

$(WISPKIT)\acu\wispicon.ico:		$(BASEDIR)\acu\$(@F)
	$(COPY) $** $@

$(WISPKIT)\acu\acu50\sub85.c:			$(BASEDIR)\acu\sub85_acu50.c
	$(COPY) $** $@

$(WISPKIT)\acu\acu50\wisprts.rc:		$(BASEDIR)\acu\$(@F)
	$(COPY) $** $@

$(WISPKIT)\acu\acu50\wrun32wisp_acu50.mak:	$(BASEDIR)\acu\$(@F)
	$(COPY) $** $@

$(WISPKIT)\acu\acu51\sub85.c:			$(BASEDIR)\acu\sub85_acu51.c
	$(COPY) $** $@

$(WISPKIT)\acu\acu51\wisprts.rc:		$(BASEDIR)\acu\$(@F)
	$(COPY) $** $@

$(WISPKIT)\acu\acu51\wrun32wisp_acu51.mak:	$(BASEDIR)\acu\$(@F)
	$(COPY) $** $@

$(WISPKIT)\acu\acu52\sub85.c:			$(BASEDIR)\acu\sub85_acu52.c
	$(COPY) $** $@

$(WISPKIT)\acu\acu52\wisprts.rc:		$(BASEDIR)\acu\$(@F)
	$(COPY) $** $@

$(WISPKIT)\acu\acu52\wrun32wisp_acu52.mak:	$(BASEDIR)\acu\$(@F)
	$(COPY) $** $@

$(WISPKIT)\acu\acu52\run32wisp_acu52.mak:	$(BASEDIR)\acu\$(@F)
	$(COPY) $** $@

$(WISPKIT)\acu\acu60\build_wisp_acu60_rts.txt:	$(BASEDIR)\acu\acu60\$(@F)
	$(COPY) $** $@

$(WISPKIT)\acu\acu60\build_wisp_acu60atm_rts.txt:	$(BASEDIR)\acu\acu60\$(@F)
	$(COPY) $** $@

$(WISPKIT)\acu\acu60\sub85.c:			$(BASEDIR)\acu\sub85_acu60.c
	$(COPY) $** $@

$(WISPKIT)\acu\acu60\wrundll.rc:		$(BASEDIR)\acu\acu60\$(@F)
	$(COPY) $** $@

$(WISPKIT)\acu\acu60\wrundll_wisp_acu60.mak:	$(BASEDIR)\acu\acu60\$(@F)
	$(COPY) $** $@

$(WISPKIT)\acu\acu60\wrundll_wisp_acu60.dsp:	$(BASEDIR)\acu\acu60\$(@F)
	$(COPY) $** $@

$(WISPKIT)\acu\acu60\run32.rc:			$(BASEDIR)\acu\acu60\$(@F)
	$(COPY) $** $@

$(WISPKIT)\acu\acu60\run32_wisp_acu60.mak:	$(BASEDIR)\acu\acu60\$(@F)
	$(COPY) $** $@

$(WISPKIT)\acu\acu60\run32_wisp_acu60.dsp:	$(BASEDIR)\acu\acu60\$(@F)
	$(COPY) $** $@



$(WISPKIT)\acu\acu62\build_wisp_acu62_rts.txt:	$(BASEDIR)\acu\acu62\$(@F)
	$(COPY) $** $@

$(WISPKIT)\acu\acu62\sub85.c:			$(BASEDIR)\acu\sub85_acu62.c
	$(COPY) $** $@

$(WISPKIT)\acu\acu62\wisp.rc:			$(BASEDIR)\acu\acu62\$(@F)
	$(COPY) $** $@

$(WISPKIT)\acu\acu62\wrundll.vcproj:		$(BASEDIR)\acu\acu62\$(@F)
	$(COPY) $** $@

$(WISPKIT)\acu\acu72\build_wisp_acu72_rts.txt:	$(BASEDIR)\acu\acu72\$(@F)
	$(COPY) $** $@

$(WISPKIT)\acu\acu72\sub85.c:			$(BASEDIR)\acu\sub85_acu72.c
	$(COPY) $** $@

$(WISPKIT)\acu\acu72\wisp.rc:			$(BASEDIR)\acu\acu72\$(@F)
	$(COPY) $** $@

$(WISPKIT)\acu\acu72\wrundll.vcproj:		$(BASEDIR)\acu\acu72\$(@F)
	$(COPY) $** $@

$(WISPKIT)\acu\acu72\build_wisp_acu72_atm_rts.txt:	$(BASEDIR)\acu\acu72\$(@F)
	$(COPY) $** $@

$(WISPKIT)\acu\acu72\run32.vcproj:		$(BASEDIR)\acu\acu72\$(@F)
	$(COPY) $** $@


$(WISPKIT)\acu\acu81\build_wisp_acu81_rts.txt:	$(BASEDIR)\acu\acu81\$(@F)
	$(COPY) $** $@

$(WISPKIT)\acu\acu81\build_wisp_acu81_atm_rts.txt:	$(BASEDIR)\acu\acu81\$(@F)
	$(COPY) $** $@

$(WISPKIT)\acu\acu81\sub85.c:			$(BASEDIR)\acu\sub85_acu81.c
	$(COPY) $** $@

$(WISPKIT)\acu\acu81\wisp.rc:			$(BASEDIR)\acu\acu81\$(@F)
	$(COPY) $** $@

$(WISPKIT)\acu\acu81\wrundll.rc:		$(BASEDIR)\acu\acu81\$(@F)
	$(COPY) $** $@

$(WISPKIT)\acu\acu81\wrundll.vcproj:	$(BASEDIR)\acu\acu81\$(@F)
	$(COPY) $** $@

$(WISPKIT)\acu\acu81\run32.vcproj:		$(BASEDIR)\acu\acu81\$(@F)
	$(COPY) $** $@


$(WISPKIT)\demo\cursor.wps:	$(BASEDIR)\wproc\$(@F)
	$(COPY) $** $@

$(WISPKIT)\demo\demo.wps:	$(BASEDIR)\wproc\$(@F)
	$(COPY) $** $@

$(WISPKIT)\demo\dr.wps:		$(BASEDIR)\wproc\$(@F)
	$(COPY) $** $@

$(WISPKIT)\demo\ed.wpr:		$(BASEDIR)\wproc\$(@F)
	$(COPY) $** $@

$(WISPKIT)\demo\ed.wps:		$(BASEDIR)\wproc\$(@F)
	$(COPY) $** $@

$(WISPKIT)\demo\environ.wps:	$(BASEDIR)\wproc\$(@F)
	$(COPY) $** $@

$(WISPKIT)\demo\putparm.wps:	$(BASEDIR)\wproc\$(@F)
	$(COPY) $** $@

$(WISPKIT)\demo\screen.wps:	$(BASEDIR)\wproc\$(@F)
	$(COPY) $** $@

$(WISPKIT)\demo\test.wps:	$(BASEDIR)\wproc\$(@F)
	$(COPY) $** $@

$(WISPKIT)\demo\video.wps:	$(BASEDIR)\wproc\$(@F)
	$(COPY) $** $@

$(WISPKIT)\demo\qabckgrd.wcb:	$(BASEDIR)\test\$(@F)
	$(COPY) $** $@

$(WISPKIT)\wispntsetup.txt:	$(BASEDIR)\doc\$(@F)
	$(COPY) $** $@

#
#	ede
#

$(EDEKIT)\good.exe:		$(BASEDIR)\bin\$(@F)
	$(COPY) $** $@

$(EDEKIT)\ede.lib:		$(BASEDIR)\ede\$(@F)
	$(COPY) $** $@

$(EDEKIT)\edem.lib:		$(BASEDIR)\ede\$(@F)
	$(COPY) $** $@

$(EDEKIT)\edentsetup.txt:	$(BASEDIR)\ede\$(@F)
	$(COPY) $** $@

$(EDEKIT)\wrun32wisp_ede_acu50.mak:	$(BASEDIR)\acu\$(@F)
	$(COPY) $** $@

$(EDEKIT)\wrun32wisp_ede_acu51.mak:	$(BASEDIR)\acu\$(@F)
	$(COPY) $** $@

$(EDEKIT)\wrun32wisp_ede_acu52.mak:	$(BASEDIR)\acu\$(@F)
	$(COPY) $** $@

$(EDEKIT)\build_ede_acu60_rts.txt:	$(BASEDIR)\acu\acu60\$(@F)
	$(COPY) $** $@

$(EDEKIT)\wrundll_ede_acu60.mak:	$(BASEDIR)\acu\acu60\$(@F)
	$(COPY) $** $@

$(EDEKIT)\wrundll_ede_acu60.dsp:	$(BASEDIR)\acu\acu60\$(@F)
	$(COPY) $** $@


$(EDEKIT)\demo\helpmap.dat:	$(BASEDIR)\ede\demo\$(@F)
	$(COPY) $** $@

$(EDEKIT)\demo\mcbbld.wcb:	$(BASEDIR)\ede\demo\$(@F)
	$(COPY) $** $@

$(EDEKIT)\demo\mcbcpy1.wcb:	$(BASEDIR)\ede\demo\$(@F)
	$(COPY) $** $@

$(EDEKIT)\demo\mcbcpy2.wcb:	$(BASEDIR)\ede\demo\$(@F)
	$(COPY) $** $@

$(EDEKIT)\demo\mcbcpy3.wcb:	$(BASEDIR)\ede\demo\$(@F)
	$(COPY) $** $@

$(EDEKIT)\demo\mcbedit.wcb:	$(BASEDIR)\ede\demo\$(@F)
	$(COPY) $** $@

$(EDEKIT)\demo\menudefs.wcb:	$(BASEDIR)\ede\demo\$(@F)
	$(COPY) $** $@

$(EDEKIT)\demo\menudemo.wcb:	$(BASEDIR)\ede\demo\$(@F)
	$(COPY) $** $@

$(EDEKIT)\demo\menudisp.wcb:	$(BASEDIR)\ede\demo\$(@F)
	$(COPY) $** $@

$(EDEKIT)\demo\menulogo.wcb:	$(BASEDIR)\ede\demo\$(@F)
	$(COPY) $** $@

$(EDEKIT)\demo\menumcbs.wcb:	$(BASEDIR)\ede\demo\$(@F)
	$(COPY) $** $@

$(EDEKIT)\demo\menuvect.wcb:	$(BASEDIR)\ede\demo\$(@F)
	$(COPY) $** $@

$(EDEKIT)\demo\mcbcode.hlp:	$(BASEDIR)\ede\demo\$(@F)
	$(COPY) $** $@

$(EDEKIT)\demo\mcbcol.hlp:	$(BASEDIR)\ede\demo\$(@F)
	$(COPY) $** $@

$(EDEKIT)\demo\mcbdisp.hlp:	$(BASEDIR)\ede\demo\$(@F)
	$(COPY) $** $@

$(EDEKIT)\demo\mcbedit.hlp:	$(BASEDIR)\ede\demo\$(@F)
	$(COPY) $** $@

$(EDEKIT)\demo\mcbicnt.hlp:	$(BASEDIR)\ede\demo\$(@F)
	$(COPY) $** $@

$(EDEKIT)\demo\mcbname.hlp:	$(BASEDIR)\ede\demo\$(@F)
	$(COPY) $** $@

$(EDEKIT)\demo\mcbopts.hlp:	$(BASEDIR)\ede\demo\$(@F)
	$(COPY) $** $@

$(EDEKIT)\demo\mcbrow.hlp:	$(BASEDIR)\ede\demo\$(@F)
	$(COPY) $** $@

$(EDEKIT)\demo\mcbtext.hlp:	$(BASEDIR)\ede\demo\$(@F)
	$(COPY) $** $@

$(EDEKIT)\demo\mcbtype.hlp:	$(BASEDIR)\ede\demo\$(@F)
	$(COPY) $** $@

$(EDEKIT)\demo\mcbvalue.hlp:	$(BASEDIR)\ede\demo\$(@F)
	$(COPY) $** $@

$(EDEKIT)\demo\mcbwidth.hlp:	$(BASEDIR)\ede\demo\$(@F)
	$(COPY) $** $@

$(EDEKIT)\demo\menudemo.opt:	$(BASEDIR)\ede\demo\$(@F)
	$(COPY) $** $@

$(EDEKIT)\demo\menudemo.mak:	$(BASEDIR)\ede\demo\$(@F)
	$(COPY) $** $@


#
#	kcsiacu
#

$(KCSIACUKIT)\CONTROL:		$(BASEDIR)\kcsi\$(@F)
	$(COPY) $** $@

$(KCSIACUKIT)\DATENTRY:		$(BASEDIR)\kcsi\$(@F)
	$(COPY) $** $@

$(KCSIACUKIT)\INQUIRY:		$(BASEDIR)\kcsi\$(@F)
	$(COPY) $** $@

$(KCSIACUKIT)\REPORT:		$(BASEDIR)\kcsi\$(@F)
	$(COPY) $** $@

$(KCSIACUKIT)\CREATE:		$(BASEDIR)\kcsi\$(@F)
	$(COPY) $** $@

$(KCSIACUKIT)\kcsiacu.lib:	$(BASEDIR)\kcsi\$(@F)
	$(COPY) $** $@

$(KCSIACUKIT)\kcsi_sub85_inc.c:	$(BASEDIR)\kcsi\$(@F)
	$(COPY) $** $@

$(KCSIACUKIT)\ctlcnvrt.wcb:	$(BASEDIR)\kcsi\$(@F)
	$(COPY) $** $@

$(KCSIACUKIT)\rptcnvrt.wcb:	$(BASEDIR)\kcsi\$(@F)
	$(COPY) $** $@

$(KCSIACUKIT)\wrun32wisp_kcsi_acu50.mak:	$(BASEDIR)\acu\$(@F)
	$(COPY) $** $@

$(KCSIACUKIT)\wrun32wisp_kcsi_acu51.mak:	$(BASEDIR)\acu\$(@F)
	$(COPY) $** $@

$(KCSIACUKIT)\wrun32wisp_kcsi_acu52.mak:	$(BASEDIR)\acu\$(@F)
	$(COPY) $** $@

$(KCSIACUKIT)\build_kcsi_acu60_rts.txt:		$(BASEDIR)\acu\acu60\$(@F)
	$(COPY) $** $@

$(KCSIACUKIT)\wrundll_kcsi_acu60.mak:		$(BASEDIR)\acu\acu60\$(@F)
	$(COPY) $** $@

$(KCSIACUKIT)\wrundll_kcsi_acu60.dsp:		$(BASEDIR)\acu\acu60\$(@F)
	$(COPY) $** $@


$(KCSIACUKIT)\build_kcsi_acu62_rts.txt:		$(BASEDIR)\acu\acu62\$(@F)
	$(COPY) $** $@

$(KCSIACUKIT)\acu62\wrundll.vcproj:		$(BASEDIR)\acu\acu62\kcsi\$(@F)
	$(COPY) $** $@

$(KCSIACUKIT)\build_kcsi_acu72_rts.txt:		$(BASEDIR)\acu\acu72\$(@F)
	$(COPY) $** $@

$(KCSIACUKIT)\acu72\wrundll.vcproj:		$(BASEDIR)\acu\acu72\kcsi\$(@F)
	$(COPY) $** $@


$(KCSIACUKIT)\kcsintsetup.txt:	$(BASEDIR)\kcsi\$(@F)
	$(COPY) $** $@

$(KCSIACUKIT)\kcsi_relnotes.txt:	$(BASEDIR)\kcsi\$(@F)
	$(COPY) $** $@

$(KCSIACUKIT)\kcsi_packlist.txt:	$(BASEDIR)\kcsi\$(@F)
	$(COPY) $** $@


#
#	WISPKIT components
#

#
#	sample config components
#

$(BASEDIR)\config\options:	$(BASEDIR)\etc\$(@F)
	$(COPY) $** $@

$(BASEDIR)\config\w4wmap:	$(BASEDIR)\etc\$(@F)
	$(COPY) $** $@

$(BASEDIR)\config\acuconfig:	$(BASEDIR)\nt\$(@F)
	$(COPY) $** $@

$(BASEDIR)\config\charmap:	$(BASEDIR)\nt\$(@F)
	$(COPY) $** $@

$(BASEDIR)\config\cqmap:	$(BASEDIR)\nt\$(@F)
	$(COPY) $** $@

$(BASEDIR)\config\forms:	$(BASEDIR)\nt\$(@F)
	$(COPY) $** $@

$(BASEDIR)\config\lgmap:	$(BASEDIR)\nt\$(@F)
	$(COPY) $** $@

$(BASEDIR)\config\lpmap:	$(BASEDIR)\nt\$(@F)
	$(COPY) $** $@

$(BASEDIR)\config\prmap:	$(BASEDIR)\nt\$(@F)
	$(COPY) $** $@

$(BASEDIR)\config\wrun.cfg:	$(BASEDIR)\nt\$(@F)
	$(COPY) $** $@

$(BASEDIR)\config\wsysconf.cfg:	$(BASEDIR)\nt\$(@F)
	$(COPY) $** $@

$(BASEDIR)\config\wproc.msg:	$(BASEDIR)\wproc\$(@F)
	$(COPY) $** $@

$(BASEDIR)\config\videocap\wincon.vcap: $(BASEDIR)\videocap\$(@F)
	$(COPY) $** $@

$(BASEDIR)\config\videocap\xterm.vcap: $(BASEDIR)\videocap\$(@F)
	$(COPY) $** $@

$(BASEDIR)\config\videocap\ansi.vcap: $(BASEDIR)\videocap\$(@F)
	$(COPY) $** $@

#
#	misc components
#
# $(BASEDIR)\wproc\ed.wpr: $(BASEDIR)\wproc\ed.wps $(BASEDIR)\bin\wproc.exe
#	$(BASEDIR)\bin\wproc.exe -c $(BASEDIR)\wproc\ed.wps
