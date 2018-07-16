# $Id:$
#
ACUDIR=C:\Acucorp\ACUCBL520\ACUGT
WISPDIR=C:\dev\wisp\src\kit\wisp
ACUMAK=wrun32wisp_acu52.mak
RTS=wrun32wisp
BLDDIR=bld_wisp

default: rts

header: $(WISPDIR) $(ACUDIR) 
	@echo ">>>> BUILDING ACUCOBOL RUNTIME"
	@echo ">>>>"
	@echo ">>>> WISPDIR   = " $(WISPDIR)
	@echo ">>>> ACUDIR    = " $(ACUDIR)
	@echo ">>>> ACUMAK    = " $(ACUMAK)
	@echo ">>>> CD        = " 
	@CD
	@echo ">>>>"


rts: header copystuff
	cd $(BLDDIR)
	$(MAKE) /f $(ACUMAK) WISPDIR=$(WISPDIR)
	copy $(ACUDIR)\bin\wrun32.alc $(RTS).alc

copystuff:
	-rmdir /Q /S $(BLDDIR)
	mkdir $(BLDDIR)
	copy $(ACUDIR)\lib\* $(BLDDIR)
	copy $(ACUDIR)\bin\*.dll $(BLDDIR)
	copy $(WISPDIR)\acu\wisp_sub85_inc.c $(BLDDIR)
	copy $(WISPDIR)\acu\wispicon.ico $(BLDDIR)
	copy $(WISPDIR)\acu\acu52\sub85.c $(BLDDIR)
	copy $(WISPDIR)\acu\acu52\wisprts.rc $(BLDDIR)
	copy $(WISPDIR)\acu\acu52\$(ACUMAK) $(BLDDIR)
