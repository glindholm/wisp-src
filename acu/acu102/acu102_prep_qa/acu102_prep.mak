# acu102_prep.mak
#
#ACUDIR=C:\data\Acucorp\ACUCBL1020\ACUGT
#WISPSRC=..\..\..
WISPKITDIR=$(WISPSRC)\kit\wisp
KCSIKITDIR=$(WISPSRC)\kit\kcsiacu
BLDDIR_WISP=acu_lib_wisp
BLDDIR_KCSI=acu_lib_kcsi

default: prepare

header: $(WISPKITDIR) $(ACUDIR) 
	@echo ">>>> PREPARING TO BUILD ACUCOBOL 10.2 RUNTIMES"
	@echo ">>>>"
	@echo ">>>> WISPSRC    = " $(WISPSRC)
	@echo ">>>> WISPKITDIR = " $(WISPKITDIR)
	@echo ">>>> KCSIKITDIR = " $(KCSIKITDIR)
	@echo ">>>> ACUDIR     = " $(ACUDIR)
	@echo ">>>> CD         = " 
	@CD
	@echo ">>>>"


#prepare:  header prep_wisp prep_kcsi
prepare:  header prep_wisp 

prep_wisp: clean_wisp
	mkdir $(BLDDIR_WISP)
	copy $(ACUDIR)\lib\* $(BLDDIR_WISP)
	copy $(WISPKITDIR)\lib\wisp.lib $(BLDDIR_WISP)
	copy $(WISPKITDIR)\lib\video.lib $(BLDDIR_WISP)
	copy $(WISPKITDIR)\acu\wisp_sub85_inc.c $(BLDDIR_WISP)
	copy $(WISPKITDIR)\acu\wispicon.ico $(BLDDIR_WISP)
	copy $(WISPKITDIR)\acu\acu102\sub85.c $(BLDDIR_WISP)
	copy $(WISPKITDIR)\acu\acu102\wisp.rc $(BLDDIR_WISP)
	copy $(WISPKITDIR)\acu\acu102\wrundll.vcxproj $(BLDDIR_WISP)
	copy $(WISPKITDIR)\acu\acu102\run32.vcxproj $(BLDDIR_WISP)
	copy $(WISPKITDIR)\acu\acu102\crun32.vcxproj $(BLDDIR_WISP)


prep_kcsi: clean_kcsi
	mkdir $(BLDDIR_KCSI)
	copy $(ACUDIR)\lib\* $(BLDDIR_KCSI)
	copy $(WISPKITDIR)\lib\wisp.lib $(BLDDIR_KCSI)
	copy $(WISPKITDIR)\lib\video.lib $(BLDDIR_KCSI)
	copy $(WISPKITDIR)\acu\wisp_sub85_inc.c $(BLDDIR_KCSI)
	copy $(WISPKITDIR)\acu\wispicon.ico $(BLDDIR_KCSI)
	copy $(WISPKITDIR)\acu\acu102\sub85.c $(BLDDIR_KCSI)
	copy $(WISPKITDIR)\acu\acu102\wisp.rc $(BLDDIR_KCSI)
	copy $(KCSIKITDIR)\kcsiacu.lib $(BLDDIR_KCSI)
	copy $(KCSIKITDIR)\kcsi_sub85_inc.c $(BLDDIR_KCSI)
	copy $(KCSIKITDIR)\acu102\wrundll.vcxproj $(BLDDIR_KCSI)
	copy $(KCSIKITDIR)\acu102\crun32.vcxproj $(BLDDIR_KCSI)
	copy $(KCSIKITDIR)\acu102\run32.vcxproj $(BLDDIR_KCSI)


clean: clean_wisp clean_kcsi

clean_wisp:
	-rmdir /Q /S $(BLDDIR_WISP)

clean_kcsi:
	-rmdir /Q /S $(BLDDIR_KCSI)

