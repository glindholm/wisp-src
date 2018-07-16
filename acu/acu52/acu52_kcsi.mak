# $Id:$
#
ACUDIR=C:\Acucorp\ACUCBL520\ACUGT
WISPDIR=C:\dev\wisp\src\kit\wisp
KCSIDIR=C:\dev\wisp\src\kit\kcsiacu
ACUMAK=wrun32wisp_kcsi_acu52.mak
RTS=wrun32wispk
BLDDIR=bld_kcsi

default: rts

header: $(WISPDIR) $(ACUDIR) 
	@echo ">>>> BUILDING ACUCOBOL RUNTIME"
	@echo ">>>>"
	@echo ">>>> WISPDIR   = " $(WISPDIR)
	@echo ">>>> KCSIDIR   = " $(KCSIDIR)
	@echo ">>>> ACUDIR    = " $(ACUDIR)
	@echo ">>>> ACUMAK    = " $(ACUMAK)
	@echo ">>>> CD        = " 
	@CD
	@echo ">>>>"


rts: header copystuff
	cd $(BLDDIR)
	$(MAKE) /f $(ACUMAK) WISPDIR=$(WISPDIR) KCSIDIR=$(KCSIDIR)
	copy $(ACUDIR)\bin\wrun32.alc $(RTS).alc

copystuff:
	-rmdir /Q /S $(BLDDIR)
	mkdir $(BLDDIR)
	copy $(ACUDIR)\lib\* $(BLDDIR)
	copy $(ACUDIR)\bin\*.dll $(BLDDIR)
	copy $(KCSIDIR)\$(ACUMAK) $(BLDDIR)
	copy $(KCSIDIR)\kcsi_sub85_inc.c $(BLDDIR)
	copy $(WISPDIR)\acu\wisp_sub85_inc.c $(BLDDIR)
	copy $(WISPDIR)\acu\wispicon.ico $(BLDDIR)
	copy $(WISPDIR)\acu\acu52\sub85.c $(BLDDIR)
	copy $(WISPDIR)\acu\acu52\wisprts.rc $(BLDDIR)
