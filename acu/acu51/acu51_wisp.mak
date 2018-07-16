# $Id:$
#
ACUDIR=C:\Acucorp\ACUCBL510\ACUGT
WISPDIR=c:\dev\wisp\src\kit\wisp
ACUMAK=wrun32wisp_acu51.mak
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
	@rem copy $(ACUDIR)\bin\wrun32.alc $(RTS).alc

copystuff:
	-rmdir /Q /S $(BLDDIR)
	mkdir $(BLDDIR)
	copy $(ACUDIR)\lib\* $(BLDDIR)
	copy $(ACUDIR)\bin\*.dll $(BLDDIR)
	copy $(WISPDIR)\acu\wisp_sub85_inc.c $(BLDDIR)
	copy $(WISPDIR)\acu\wispicon.ico $(BLDDIR)
	copy $(WISPDIR)\acu\acu51\sub85.c $(BLDDIR)
	copy $(WISPDIR)\acu\acu51\wisprts.rc $(BLDDIR)
	copy $(WISPDIR)\acu\acu51\$(ACUMAK) $(BLDDIR)
