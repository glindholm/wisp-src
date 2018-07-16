# $Id:$
#
ACUDIR=C:\Acucorp\ACUCBL520\ACUGT
WISPDIR=C:\dev\wisp\src\kit\wisp
EDEDIR=C:\dev\wisp\src\kit\ede
ACUMAK=wrun32wisp_ede_acu52.mak
RTS=wrun32wispe
BLDDIR=bld_ede

default: rts

header: $(WISPDIR) $(ACUDIR) 
	@echo ">>>> BUILDING ACUCOBOL RUNTIME"
	@echo ">>>>"
	@echo ">>>> WISPDIR   = " $(WISPDIR)
	@echo ">>>> EDEDIR    = " $(EDEDIR)
	@echo ">>>> ACUDIR    = " $(ACUDIR)
	@echo ">>>> ACUMAK    = " $(ACUMAK)
	@echo ">>>> CD        = " 
	@CD
	@echo ">>>>"


rts: header copystuff
	cd $(BLDDIR)
	$(MAKE) /f $(ACUMAK) WISPDIR=$(WISPDIR) EDEDIR=$(EDEDIR)
	copy $(ACUDIR)\bin\wrun32.alc $(RTS).alc

copystuff:
	-rmdir /Q /S $(BLDDIR)
	mkdir $(BLDDIR)
	copy $(ACUDIR)\lib\*			$(BLDDIR)
	copy $(ACUDIR)\bin\*.dll		$(BLDDIR)
	copy $(EDEDIR)\$(ACUMAK)		$(BLDDIR)
	copy $(WISPDIR)\acu\wisp_sub85_inc.c	$(BLDDIR)
	copy $(WISPDIR)\acu\wispicon.ico	$(BLDDIR)
	copy $(WISPDIR)\acu\acu52\sub85.c	$(BLDDIR)
	copy $(WISPDIR)\acu\acu52\wisprts.rc	$(BLDDIR)

