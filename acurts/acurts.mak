# acurts.mak
#
ACUDIR=C:\Acucorp\ACUCBL520\ACUGT
ACULIB=$(ACUDIR)\lib
WISPDIR=C:\dev\wisp\src
ACUMAK=wrun32wisp_acu52.mak
RTS=wrun32wisp

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
	$(MAKE) /f $(ACUMAK) WISPDIR=$(WISPDIR) clean
	$(MAKE) /f $(ACUMAK) WISPDIR=$(WISPDIR)
	copy $(ACUDIR)\bin\wrun32.alc $(RTS).alc

copystuff:
	-del /F /Q *.exe *.obj *.dll *.lib
	copy $(ACULIB)\* 
	copy $(WISPDIR)\acu\$(ACUMAK) 
	copy $(WISPDIR)\acu\sub85.c 
	copy $(WISPDIR)\acu\wisprts.rc 
	copy $(WISPDIR)\acu\wispicon.ico 

