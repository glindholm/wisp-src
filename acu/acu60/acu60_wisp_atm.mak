# $Id:$
#
ACUDIR=C:\Acucorp\ACUCBL600_ATM\ACUGT
WISPDIR=C:\dev\wisp\src\kit\wisp
BLDDIR=bld_wisp_atm

default: rts

header: $(WISPDIR) $(ACUDIR) 
	@echo ">>>> BUILDING ACUCOBOL 6.0 (ATM) WISP RUNTIME"
	@echo ">>>>"
	@echo ">>>> WISPDIR   = " $(WISPDIR)
	@echo ">>>> ACUDIR    = " $(ACUDIR)
	@echo ">>>> CD        = " 
	@CD
	@echo ">>>>"


rts: header copystuff
	cd $(BLDDIR)
	$(MAKE) /f run32_wisp_acu60.mak
	copy run32_wisp.exe $(ACUDIR)\bin

copystuff:
	-rmdir /Q /S $(BLDDIR)
	mkdir $(BLDDIR)
	copy $(ACUDIR)\lib\* $(BLDDIR)
	copy $(WISPDIR)\lib\wisp.lib $(BLDDIR)
	copy $(WISPDIR)\lib\video.lib $(BLDDIR)
	copy $(WISPDIR)\acu\wisp_sub85_inc.c $(BLDDIR)
	copy $(WISPDIR)\acu\wispicon.ico $(BLDDIR)
	copy $(WISPDIR)\acu\acu60\sub85.c $(BLDDIR)
	copy $(WISPDIR)\acu\acu60\run32.rc $(BLDDIR)
	copy $(WISPDIR)\acu\acu60\run32_wisp_acu60.mak $(BLDDIR)
	copy $(WISPDIR)\acu\acu60\run32_wisp_acu60.dsp $(BLDDIR)
