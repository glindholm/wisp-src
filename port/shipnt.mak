#
#	shipkit.mak
#

COPY=copy

BASEDIR=..

KITDIR=		$(BASEDIR)\kit

WISPKIT=	$(KITDIR)\wisp
EDEKIT=		$(KITDIR)\ede
CRIDACUKIT=	$(KITDIR)\cridacu
CREATEACUKIT=	$(KITDIR)\createacu
RTSKIT=		$(KITDIR)\rts
VHEADKIT=	$(KITDIR)\v

wisp_dist_list=	$(WISPKIT)\bin\display.exe	\
		$(WISPKIT)\bin\hexed.exe	\
		$(WISPKIT)\bin\makemsg.exe 	\
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
		$(WISPKIT)\lib\videom.lib	\
		$(WISPKIT)\lib\wisp.lib		\
		$(WISPKIT)\lib\wispm.lib	\
		$(WISPKIT)\etc\words.def	\
		$(WISPKIT)\etc\disprint.wcb	\
		$(WISPKIT)\etc\disprint.mak	\
		$(WISPKIT)\etc\wispplat.wcb	\
		$(WISPKIT)\etc\softlink.wcb	\
		$(WISPKIT)\etc\wproc.txt	\
		$(WISPKIT)\etc\wisp_relnotes.txt	\
		$(WISPKIT)\etc\aqmwisp.txt	\
		$(WISPKIT)\etc\nonascii.txt	\
		$(WISPKIT)\etc\vcolors.txt	\
		$(WISPKIT)\etc\nttelnet.txt	\
		$(WISPKIT)\config\ACUCONFIG	\
		$(WISPKIT)\config\CHARMAP	\
		$(WISPKIT)\config\CQMAP		\
		$(WISPKIT)\config\FORMS		\
		$(WISPKIT)\config\LGMAP		\
		$(WISPKIT)\config\LPMAP		\
		$(WISPKIT)\config\OPTIONS	\
		$(WISPKIT)\config\PRMAP		\
		$(WISPKIT)\config\W4WMAP	\
		$(WISPKIT)\config\wispmsg.dat	\
		$(WISPKIT)\config\wispmsg.txt	\
		$(WISPKIT)\config\wproc.msg	\
		$(WISPKIT)\config\wrun.cfg	\
		$(WISPKIT)\config\wsysconf.cfg 	\
		$(WISPKIT)\config\videocap\wincon.vcap 	\
		$(WISPKIT)\config\videocap\xterm.vcap 	\
		$(WISPKIT)\config\videocap\ansi.vcap 	\
		$(WISPKIT)\acu\aculink.wcb	\
		$(WISPKIT)\acu\ACULINK		\
		$(WISPKIT)\acu\acuusing.cob	\
		$(WISPKIT)\acu\ACUUSING		\
		$(WISPKIT)\acu\sub85.c		\
		$(WISPKIT)\acu\wwruncbl.mak	\
		$(WISPKIT)\acu\wrun32wisp_acu51.mak	\
		$(WISPKIT)\acu\wrun32wisp_acu52.mak	\
		$(WISPKIT)\acu\wacuerror.cob		\
		$(WISPKIT)\acu\wacudisplay.cob		\
		$(WISPKIT)\acu\wacufac2screen.cob	\
		$(WISPKIT)\acu\wacugetparm.cob		\
		$(WISPKIT)\acu\wacugetpfkey.cob		\
		$(WISPKIT)\acu\wacuhelp.cob		\
		$(WISPKIT)\acu\wacuwsb.cob		\
		$(WISPKIT)\acu\wispacn.txt		\
		$(WISPKIT)\acu\wispicon.ico		\
		$(WISPKIT)\acu\wisprts.rc		\
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
		$(WISPKIT)\wispntdoc.txt	\
		$(WISPKIT)\wispntsetup.txt

ede_dist_list=	$(EDEKIT)\good.exe		\
		$(EDEKIT)\ede.lib		\
		$(EDEKIT)\edem.lib		\
		$(EDEKIT)\edentsetup.txt	\
		$(EDEKIT)\wwruncbl.mak		\
		$(EDEKIT)\wrun32wisp_ede_acu51.mak	\
		$(EDEKIT)\wrun32wisp_ede_acu52.mak	\
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

cridacu_dist_list= \
		$(CRIDACUKIT)\CONTROL		\
		$(CRIDACUKIT)\DATENTRY		\
		$(CRIDACUKIT)\INQUIRY		\
		$(CRIDACUKIT)\REPORT		\
		$(CRIDACUKIT)\cridacu.lib	\
		$(CRIDACUKIT)\cridacum.lib	\
		$(CRIDACUKIT)\crid.h		\
		$(CRIDACUKIT)\crid85.c		\
		$(CRIDACUKIT)\cridtbl.c		\
		$(CRIDACUKIT)\ctlcnvrt.wcb	\
		$(CRIDACUKIT)\rptcnvrt.wcb	\
		$(CRIDACUKIT)\wwruncbl.mak	\
		$(CRIDACUKIT)\wrun32wisp_crid_acu51.mak	\
		$(CRIDACUKIT)\wrun32wisp_crid_acu52.mak	\
		$(CRIDACUKIT)\crid_relnotes.txt	\
		$(CRIDACUKIT)\crid_packlist.txt	\
		$(CRIDACUKIT)\cridntsetup.txt	

createacu_dist_list= \
		$(CREATEACUKIT)\create.exe	\
		$(CREATEACUKIT)\createacu.lib	\
		$(CREATEACUKIT)\vscrmain.obj	\
		$(CREATEACUKIT)\wwruncbl.mak	\
		$(CREATEACUKIT)\create_relnotes.txt	\
		$(CREATEACUKIT)\create_packlist.txt	\
		$(CREATEACUKIT)\createntsetup.txt	

rts_dist_list= \
		$(RTSKIT)\wwruncbl.exe		\
		$(RTSKIT)\wwruncble.exe		\
		$(RTSKIT)\wwruncblk.exe		\
		$(RTSKIT)\wwruncblke.exe

#
#	targets
#
default: 	wispshipkit edeshipkit cridacushipkit createacushipkit

wispshipkit:	header wispkitdirs configdirs $(wisp_dist_list)

edeshipkit:	header edekitdirs $(ede_dist_list)

cridacushipkit:	header cridacukitdirs $(cridacu_dist_list)

createacushipkit: header createacukitdirs $(createacu_dist_list)

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
	$(WISPKIT)\bin			\
	$(WISPKIT)\config		\
	$(WISPKIT)\config\videocap	\
	$(WISPKIT)\demo			\
	$(WISPKIT)\etc			\
	$(WISPKIT)\lib			

wispkitdirs: $(KITDIR) $(wispkit_dir_list)

$(wispkit_dir_list):
	mkdir $@

edekit_dir_list= $(EDEKIT) $(EDEKIT)\demo

edekitdirs: $(KITDIR) $(edekit_dir_list)

$(edekit_dir_list):
	mkdir $@

cridacukitdirs: $(KITDIR) $(CRIDACUKIT)

$(CRIDACUKIT):
	mkdir $@

createacukitdirs: $(KITDIR) $(CREATEACUKIT)

$(CREATEACUKIT):
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

$(WISPKIT)\bin\makemsg.exe:	$(BASEDIR)\bin\$(@F)
	$(COPY) $** $@

$(WISPKIT)\bin\proctran.exe:	$(BASEDIR)\bin\$(@F)
	$(COPY) $** $@

$(WISPKIT)\bin\rexec.exe:	$(BASEDIR)\bin\$(@F)
	$(COPY) $** $@

$(WISPKIT)\bin\vsedit.exe:	$(BASEDIR)\bin\$(@F)
	$(COPY) $** $@

$(WISPKIT)\bin\vtest.exe:	$(BASEDIR)\bin\$(@F)
	$(COPY) $** $@

$(WISPKIT)\bin\wconfig.exe:	$(BASEDIR)\bin\$(@F)
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

$(WISPKIT)\bin\wisptran.exe:	$(BASEDIR)\bin\$(@F)
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

$(WISPKIT)\bin\wwruncbl.exe:	$(BASEDIR)\acu\$(@F)
	$(COPY) $** $@

$(WISPKIT)\lib\video.lib:	$(BASEDIR)\lib\$(@F)
	$(COPY) $** $@

$(WISPKIT)\lib\videom.lib:	$(BASEDIR)\lib\$(@F)
	$(COPY) $** $@

$(WISPKIT)\lib\wisp.lib:	$(BASEDIR)\lib\$(@F)
	$(COPY) $** $@

$(WISPKIT)\lib\wispm.lib:	$(BASEDIR)\lib\$(@F)
	$(COPY) $** $@

$(WISPKIT)\etc\words.def:	$(BASEDIR)\wisptran\$(@F)
	$(COPY) $** $@

$(WISPKIT)\etc\disprint.wcb:	$(BASEDIR)\wisputils\$(@F)
	$(COPY) $** $@

$(WISPKIT)\etc\disprint.mak:	$(BASEDIR)\wisputils\$(@F)
	$(COPY) $** $@

$(WISPKIT)\etc\wispplat.wcb:	$(BASEDIR)\wisputils\$(@F)
	$(COPY) $** $@

$(WISPKIT)\etc\softlink.wcb:	$(BASEDIR)\wisputils\$(@F)
	$(COPY) $** $@

$(WISPKIT)\etc\wproc.txt:	$(BASEDIR)\wproc\$(@F)
	$(COPY) $** $@

$(WISPKIT)\etc\wisp_relnotes.txt:	$(BASEDIR)\etc\$(@F)
	$(COPY) $** $@

$(WISPKIT)\etc\aqmwisp.txt:	$(BASEDIR)\etc\$(@F)
	$(COPY) $** $@

$(WISPKIT)\etc\nonascii.txt:	$(BASEDIR)\etc\$(@F)
	$(COPY) $** $@

$(WISPKIT)\etc\vcolors.txt:	$(BASEDIR)\etc\$(@F)
	$(COPY) $** $@

$(WISPKIT)\etc\nttelnet.txt:	$(BASEDIR)\etc\$(@F)
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

$(WISPKIT)\config\wispmsg.dat:	$(BASEDIR)\config\$(@F)
	$(COPY) $** $@

$(WISPKIT)\config\wispmsg.txt:	$(BASEDIR)\config\$(@F)
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

$(WISPKIT)\acu\aculink.wcb:	$(BASEDIR)\acu\$(@F)
	$(COPY) $** $@

$(WISPKIT)\acu\ACULINK:		$(BASEDIR)\acu\$(@F)
	$(COPY) $** $@

$(WISPKIT)\acu\acuusing.cob:	$(BASEDIR)\acu\$(@F)
	$(COPY) $** $@

$(WISPKIT)\acu\ACUUSING:	$(BASEDIR)\acu\$(@F)
	$(COPY) $** $@

$(WISPKIT)\acu\sub85.c:		$(BASEDIR)\acu\$(@F)
	$(COPY) $** $@

$(WISPKIT)\acu\wwruncbl.mak:	$(BASEDIR)\acu\$(@F)
	$(COPY) $** $@

$(WISPKIT)\acu\wrun32wisp_acu51.mak:	$(BASEDIR)\acu\$(@F)
	$(COPY) $** $@

$(WISPKIT)\acu\wrun32wisp_acu52.mak:	$(BASEDIR)\acu\$(@F)
	$(COPY) $** $@

$(WISPKIT)\acu\wacuerror.cob:		$(BASEDIR)\acu\$(@F)
	$(COPY) $** $@

$(WISPKIT)\acu\wacudisplay.cob:		$(BASEDIR)\acu\$(@F)
	$(COPY) $** $@

$(WISPKIT)\acu\wacufac2screen.cob:	$(BASEDIR)\acu\$(@F)
	$(COPY) $** $@

$(WISPKIT)\acu\wacugetparm.cob:		$(BASEDIR)\acu\$(@F)
	$(COPY) $** $@

$(WISPKIT)\acu\wacugetpfkey.cob:	$(BASEDIR)\acu\$(@F)
	$(COPY) $** $@

$(WISPKIT)\acu\wacuhelp.cob:		$(BASEDIR)\acu\$(@F)
	$(COPY) $** $@

$(WISPKIT)\acu\wacuwsb.cob:		$(BASEDIR)\acu\$(@F)
	$(COPY) $** $@

$(WISPKIT)\acu\wispacn.txt:		$(BASEDIR)\etc\$(@F)
	$(COPY) $** $@

$(WISPKIT)\acu\wispicon.ico:		$(BASEDIR)\acu\$(@F)
	$(COPY) $** $@

$(WISPKIT)\acu\wisprts.rc:		$(BASEDIR)\acu\$(@F)
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

$(WISPKIT)\demo\qabckgrd.wcb:	$(BASEDIR)\testacu\$(@F)
	$(COPY) $** $@

$(WISPKIT)\wispntdoc.txt:	$(BASEDIR)\etc\$(@F)
	$(COPY) $** $@

$(WISPKIT)\wispntsetup.txt:	$(BASEDIR)\etc\$(@F)
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

$(EDEKIT)\wwruncbl.mak:		$(BASEDIR)\acu\$(@F)
	$(COPY) $** $@

$(EDEKIT)\wrun32wisp_ede_acu51.mak:	$(BASEDIR)\acu\$(@F)
	$(COPY) $** $@

$(EDEKIT)\wrun32wisp_ede_acu52.mak:	$(BASEDIR)\acu\$(@F)
	$(COPY) $** $@

$(EDEKIT)\demo\helpmap.dat:	$(BASEDIR)\ede\$(@F)
	$(COPY) $** $@

$(EDEKIT)\demo\mcbbld.wcb:	$(BASEDIR)\ede\$(@F)
	$(COPY) $** $@

$(EDEKIT)\demo\mcbcpy1.wcb:	$(BASEDIR)\ede\$(@F)
	$(COPY) $** $@

$(EDEKIT)\demo\mcbcpy2.wcb:	$(BASEDIR)\ede\$(@F)
	$(COPY) $** $@

$(EDEKIT)\demo\mcbcpy3.wcb:	$(BASEDIR)\ede\$(@F)
	$(COPY) $** $@

$(EDEKIT)\demo\mcbedit.wcb:	$(BASEDIR)\ede\$(@F)
	$(COPY) $** $@

$(EDEKIT)\demo\menudefs.wcb:	$(BASEDIR)\ede\$(@F)
	$(COPY) $** $@

$(EDEKIT)\demo\menudemo.wcb:	$(BASEDIR)\ede\$(@F)
	$(COPY) $** $@

$(EDEKIT)\demo\menudisp.wcb:	$(BASEDIR)\ede\$(@F)
	$(COPY) $** $@

$(EDEKIT)\demo\menulogo.wcb:	$(BASEDIR)\ede\$(@F)
	$(COPY) $** $@

$(EDEKIT)\demo\menumcbs.wcb:	$(BASEDIR)\ede\$(@F)
	$(COPY) $** $@

$(EDEKIT)\demo\menuvect.wcb:	$(BASEDIR)\ede\$(@F)
	$(COPY) $** $@

$(EDEKIT)\demo\mcbcode.hlp:	$(BASEDIR)\ede\$(@F)
	$(COPY) $** $@

$(EDEKIT)\demo\mcbcol.hlp:	$(BASEDIR)\ede\$(@F)
	$(COPY) $** $@

$(EDEKIT)\demo\mcbdisp.hlp:	$(BASEDIR)\ede\$(@F)
	$(COPY) $** $@

$(EDEKIT)\demo\mcbedit.hlp:	$(BASEDIR)\ede\$(@F)
	$(COPY) $** $@

$(EDEKIT)\demo\mcbicnt.hlp:	$(BASEDIR)\ede\$(@F)
	$(COPY) $** $@

$(EDEKIT)\demo\mcbname.hlp:	$(BASEDIR)\ede\$(@F)
	$(COPY) $** $@

$(EDEKIT)\demo\mcbopts.hlp:	$(BASEDIR)\ede\$(@F)
	$(COPY) $** $@

$(EDEKIT)\demo\mcbrow.hlp:	$(BASEDIR)\ede\$(@F)
	$(COPY) $** $@

$(EDEKIT)\demo\mcbtext.hlp:	$(BASEDIR)\ede\$(@F)
	$(COPY) $** $@

$(EDEKIT)\demo\mcbtype.hlp:	$(BASEDIR)\ede\$(@F)
	$(COPY) $** $@

$(EDEKIT)\demo\mcbvalue.hlp:	$(BASEDIR)\ede\$(@F)
	$(COPY) $** $@

$(EDEKIT)\demo\mcbwidth.hlp:	$(BASEDIR)\ede\$(@F)
	$(COPY) $** $@

$(EDEKIT)\demo\menudemo.opt:	$(BASEDIR)\ede\$(@F)
	$(COPY) $** $@

$(EDEKIT)\demo\menudemo.mak:	$(BASEDIR)\ede\$(@F)
	$(COPY) $** $@


#
#	cridacu
#

$(CRIDACUKIT)\CONTROL:		$(BASEDIR)\kcsi\crid\$(@F)
	$(COPY) $** $@

$(CRIDACUKIT)\DATENTRY:		$(BASEDIR)\kcsi\crid\$(@F)
	$(COPY) $** $@

$(CRIDACUKIT)\INQUIRY:		$(BASEDIR)\kcsi\crid\$(@F)
	$(COPY) $** $@

$(CRIDACUKIT)\REPORT:		$(BASEDIR)\kcsi\crid\$(@F)
	$(COPY) $** $@

$(CRIDACUKIT)\cridacu.lib:	$(BASEDIR)\kcsi\crid\$(@F)
	$(COPY) $** $@

$(CRIDACUKIT)\cridacum.lib:	$(BASEDIR)\kcsi\crid\$(@F)
	$(COPY) $** $@

$(CRIDACUKIT)\crid.h:		$(BASEDIR)\kcsi\crid\$(@F)
	$(COPY) $** $@

$(CRIDACUKIT)\crid85.c:		$(BASEDIR)\kcsi\crid\$(@F)
	$(COPY) $** $@

$(CRIDACUKIT)\cridtbl.c:	$(BASEDIR)\kcsi\crid\$(@F)
	$(COPY) $** $@

$(CRIDACUKIT)\ctlcnvrt.wcb:	$(BASEDIR)\kcsi\crid\$(@F)
	$(COPY) $** $@

$(CRIDACUKIT)\rptcnvrt.wcb:	$(BASEDIR)\kcsi\crid\$(@F)
	$(COPY) $** $@

$(CRIDACUKIT)\wwruncbl.mak:	$(BASEDIR)\acu\$(@F)
	$(COPY) $** $@

$(CRIDACUKIT)\wrun32wisp_crid_acu51.mak:	$(BASEDIR)\acu\$(@F)
	$(COPY) $** $@

$(CRIDACUKIT)\wrun32wisp_crid_acu52.mak:	$(BASEDIR)\acu\$(@F)
	$(COPY) $** $@

$(CRIDACUKIT)\cridntsetup.txt:	$(BASEDIR)\kcsi\crid\$(@F)
	$(COPY) $** $@

$(CRIDACUKIT)\crid_relnotes.txt:	$(BASEDIR)\kcsi\crid\$(@F)
	$(COPY) $** $@

$(CRIDACUKIT)\crid_packlist.txt:	$(BASEDIR)\kcsi\crid\$(@F)
	$(COPY) $** $@


#
#	createacu
#

$(CREATEACUKIT)\create.exe:	$(BASEDIR)\kcsi\create\$(@F)
	$(COPY) $** $@

$(CREATEACUKIT)\createacu.lib:	$(BASEDIR)\kcsi\create\$(@F)
	$(COPY) $** $@

$(CREATEACUKIT)\vscrmain.obj:	$(BASEDIR)\kcsi\create\$(@F)
	$(COPY) $** $@

$(CREATEACUKIT)\wwruncbl.mak:	$(BASEDIR)\acu\$(@F)
	$(COPY) $** $@

$(CREATEACUKIT)\create_relnotes.txt:	$(BASEDIR)\kcsi\create\$(@F)
	$(COPY) $** $@

$(CREATEACUKIT)\createntsetup.txt:	$(BASEDIR)\kcsi\create\$(@F)
	$(COPY) $** $@

$(CREATEACUKIT)\create_packlist.txt:	$(BASEDIR)\kcsi\create\$(@F)
	$(COPY) $** $@

#
#	WISPKIT components
#
$(RTSKIT)\wwruncbl.exe:		$(BASEDIR)\acu\$(@F)
	$(COPY) $** $@

$(RTSKIT)\wwruncble.exe:	$(BASEDIR)\acu\$(@F)
	$(COPY) $** $@

$(RTSKIT)\wwruncblk.exe:	$(BASEDIR)\acu\$(@F)
	$(COPY) $** $@

$(RTSKIT)\wwruncblke.exe:	$(BASEDIR)\acu\$(@F)
	$(COPY) $** $@

#
#	sample config components
#
$(BASEDIR)\config\wispmsg.dat:	$(BASEDIR)\config\wispmsg.txt
	$(COPY) $(BASEDIR)\config\wispmsg.txt
	$(BASEDIR)\bin\makemsg
	$(COPY) wispmsg.dat $@

$(BASEDIR)\config\wispmsg.txt: $(BASEDIR)\etc\$(@F)
	$(COPY) $** $@

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