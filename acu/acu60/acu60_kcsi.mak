# $Id:$
#
ACUDIR=C:\Acucorp\ACUCBL600\ACUGT
WISPDIR=C:\dev\wisp\src\kit\wisp
KCSIDIR=C:\dev\wisp\src\kit\kcsiacu
BLDDIR=bld_kcsi

default: rts

header: $(WISPDIR) $(ACUDIR) 
	@echo ">>>> BUILDING ACUCOBOL 6.0 WISP+KCSI RUNTIME"
	@echo ">>>>"
	@echo ">>>> WISPDIR   = " $(WISPDIR)
	@echo ">>>> KCSIDIR   = " $(KCSIDIR)
	@echo ">>>> ACUDIR    = " $(ACUDIR)
	@echo ">>>> CD        = " 
	@CD
	@echo ">>>>"


rts: header copystuff
	cd $(BLDDIR)
	$(MAKE) /f wrundll_kcsi_acu60.mak
	copy wrun32.dll $(ACUDIR)\bin

copystuff:
	-rmdir /Q /S $(BLDDIR)
	mkdir $(BLDDIR)
	copy $(ACUDIR)\lib\* $(BLDDIR)
	copy $(WISPDIR)\lib\wisp.lib $(BLDDIR)
	copy $(WISPDIR)\lib\video.lib $(BLDDIR)
	copy $(WISPDIR)\acu\acu60\sub85.c $(BLDDIR)
	copy $(WISPDIR)\acu\acu60\wrundll.rc $(BLDDIR)
	copy $(WISPDIR)\acu\wisp_sub85_inc.c $(BLDDIR)
	copy $(WISPDIR)\acu\wispicon.ico $(BLDDIR)
	copy $(KCSIDIR)\kcsiacu.lib $(BLDDIR)
	copy $(KCSIDIR)\kcsi_sub85_inc.c $(BLDDIR)
	copy $(KCSIDIR)\wrundll_kcsi_acu60.mak $(BLDDIR)
	copy $(KCSIDIR)\wrundll_kcsi_acu60.dsp $(BLDDIR)
