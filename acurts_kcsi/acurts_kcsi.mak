# acurts_kcsi.mak
#
ACUDIR=C:\Acucorp\ACUCBL520\ACUGT
ACULIB=$(ACUDIR)\lib
WISPDIR=..\kit\wisp
KCSIDIR=..\kit\kcsiacu
ACUMAK=wrun32wisp_kcsi_acu52.mak
RTS=wrun32wispk

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
	$(MAKE) /f $(ACUMAK) WISPDIR=$(WISPDIR) KCSIDIR=$(KCSIDIR) clean
	$(MAKE) /f $(ACUMAK) WISPDIR=$(WISPDIR) KCSIDIR=$(KCSIDIR)
	copy $(ACUDIR)\bin\wrun32.alc $(RTS).alc

copystuff:
	-del /F /Q *.exe *.obj *.dll *.lib
	copy $(ACULIB)\* 
	copy $(KCSIDIR)\$(ACUMAK) 
	copy $(KCSIDIR)\kcsi_sub85_inc.c
	copy $(WISPDIR)\acu\sub85.c 
	copy $(WISPDIR)\acu\wisprts.rc 
	copy $(WISPDIR)\acu\wispicon.ico 


