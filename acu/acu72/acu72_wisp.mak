# $Id:$
#
ACUDIR=C:\Acucorp\ACUCBL722\ACUGT
WISPDIR=C:\dev\wisp\src\kit\wisp
BLDDIR=bld_wisp

default: rts

header: $(WISPDIR) $(ACUDIR) 
	@echo ">>>> BUILDING ACUCOBOL 7.2 WISP RUNTIME"
	@echo ">>>>"
	@echo ">>>> WISPDIR   = " $(WISPDIR)
	@echo ">>>> ACUDIR    = " $(ACUDIR)
	@echo ">>>> CD        = " 
	@CD
	@echo ">>>>"


rts: header copystuff
	cd $(BLDDIR)
	$(MAKE) /f wrundll_wisp_acu72.mak
	copy wrun32.dll $(ACUDIR)\bin

copystuff:
	-rmdir /Q /S $(BLDDIR)
	mkdir $(BLDDIR)
	copy $(ACUDIR)\lib\* $(BLDDIR)
	copy $(WISPDIR)\lib\wisp.lib $(BLDDIR)
	copy $(WISPDIR)\lib\video.lib $(BLDDIR)
	copy $(WISPDIR)\acu\wisp_sub85_inc.c $(BLDDIR)
	copy $(WISPDIR)\acu\wispicon.ico $(BLDDIR)
	copy $(WISPDIR)\acu\acu72\sub85.c $(BLDDIR)
	copy $(WISPDIR)\acu\acu72\wrundll.rc $(BLDDIR)
	copy $(WISPDIR)\acu\acu72\wrundll_wisp_acu72.mak $(BLDDIR)
	copy $(WISPDIR)\acu\acu72\wrundll_wisp_acu72.dsp $(BLDDIR)
