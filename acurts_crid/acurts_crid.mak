# acurts_crid.mak
#
ACUDIR=C:\Acucorp\ACUCBL520\ACUGT
ACULIB=$(ACUDIR)\lib
WISPDIR=C:\dev\wisp\src\kit\wisp
CRIDDIR=C:\dev\wisp\src\kit\cridacu
ACUMAK=wrun32wisp_crid_acu52.mak
RTS=wrun32wispc

default: rts

header: $(WISPDIR) $(ACUDIR) 
	@echo ">>>> BUILDING ACUCOBOL RUNTIME"
	@echo ">>>>"
	@echo ">>>> WISPDIR   = " $(WISPDIR)
	@echo ">>>> CRIDDIR   = " $(CRIDDIR)
	@echo ">>>> ACUDIR    = " $(ACUDIR)
	@echo ">>>> ACUMAK    = " $(ACUMAK)
	@echo ">>>> CD        = " 
	@CD
	@echo ">>>>"


rts: header copystuff
	$(MAKE) /f $(ACUMAK) WISPDIR=$(WISPDIR) CRIDDIR=$(CRIDDIR) clean
	$(MAKE) /f $(ACUMAK) WISPDIR=$(WISPDIR) CRIDDIR=$(CRIDDIR)
	copy $(ACUDIR)\bin\wrun32.alc $(RTS).alc

copystuff:
	-del /F /Q *.exe *.obj *.dll *.lib
	copy $(ACULIB)\* 
	copy $(CRIDDIR)\$(ACUMAK) 
	copy $(CRIDDIR)\crid.h
	copy $(CRIDDIR)\cridtbl.c
	copy $(CRIDDIR)\crid85.c
	copy $(WISPDIR)\acu\sub85.c 
	copy $(WISPDIR)\acu\wisprts.rc 
	copy $(WISPDIR)\acu\wispicon.ico 


