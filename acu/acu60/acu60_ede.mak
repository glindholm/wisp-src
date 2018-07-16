# $Id:$
#
ACUDIR=C:\Acucorp\ACUCBL600\ACUGT
WISPDIR=C:\dev\wisp\src\kit\wisp
EDEDIR=C:\dev\wisp\src\kit\ede
BLDDIR=bld_ede

default: rts

header: $(WISPDIR) $(ACUDIR) $(EDEDIR)
	@echo ">>>> BUILDING ACUCOBOL 6.0 WISP+EDE RUNTIME"
	@echo ">>>>"
	@echo ">>>> WISPDIR   = " $(WISPDIR)
	@echo ">>>> EDEDIR    = " $(EDEDIR)
	@echo ">>>> ACUDIR    = " $(ACUDIR)
	@echo ">>>> CD        = " 
	@CD
	@echo ">>>>"


rts: header copystuff
	cd $(BLDDIR)
	$(MAKE) /f wrundll_ede_acu60.mak
	copy wrun32.dll $(ACUDIR)\bin

copystuff:
	-rmdir /Q /S $(BLDDIR)
	mkdir $(BLDDIR)
	copy $(ACUDIR)\lib\* $(BLDDIR)
	copy $(WISPDIR)\lib\wisp.lib		$(BLDDIR)
	copy $(WISPDIR)\lib\video.lib		$(BLDDIR)
	copy $(WISPDIR)\acu\wisp_sub85_inc.c	$(BLDDIR)
	copy $(WISPDIR)\acu\wispicon.ico	$(BLDDIR)
	copy $(WISPDIR)\acu\acu60\sub85.c	$(BLDDIR)
	copy $(WISPDIR)\acu\acu60\wrundll.rc	$(BLDDIR)
	copy $(EDEDIR)\ede.lib			$(BLDDIR)
	copy $(EDEDIR)\wrundll_ede_acu60.mak	$(BLDDIR)
	copy $(EDEDIR)\wrundll_ede_acu60.dsp	$(BLDDIR)
