# acurts_ede.mak
#
ACUDIR=C:\Acucorp\ACUCBL520\ACUGT
ACULIB=$(ACUDIR)\lib
WISPDIR=C:\dev\wisp\src\kit\wisp
EDEDIR=C:\dev\wisp\src\kit\ede
ACUMAK=wrun32wisp_ede_acu52.mak
RTS=wrun32wispe

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
	$(MAKE) /f $(ACUMAK) WISPDIR=$(WISPDIR) EDEDIR=$(EDEDIR) clean
	$(MAKE) /f $(ACUMAK) WISPDIR=$(WISPDIR) EDEDIR=$(EDEDIR)
	copy $(ACUDIR)\bin\wrun32.alc $(RTS).alc

copystuff:
	-del /F /Q *.exe *.obj *.dll *.lib
	copy $(ACULIB)\* 
	copy $(EDEDIR)\$(ACUMAK) 
	copy $(WISPDIR)\acu\sub85.c 
	copy $(WISPDIR)\acu\wisprts.rc 
	copy $(WISPDIR)\acu\wispicon.ico 

