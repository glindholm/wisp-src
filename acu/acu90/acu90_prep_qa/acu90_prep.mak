# acu90_prep.mak
#
#ACUDIR=C:\data\Acucorp\ACUCBL910\ACUGT
#WISPSRC=..\..\..
WISPKITDIR=$(WISPSRC)\kit\wisp
KCSIKITDIR=$(WISPSRC)\kit\kcsiacu
EDEKITDIR=$(WISPSRC)\kit\ede
BLDDIR_WISP=acu_lib_wisp
BLDDIR_KCSI=acu_lib_kcsi
BLDDIR_EDE=acu_lib_ede

default: prepare

header: $(WISPKITDIR) $(ACUDIR) 
	@echo ">>>> PREPARING TO BUILD ACUCOBOL 9.0 RUNTIMES"
	@echo ">>>>"
	@echo ">>>> WISPSRC    = " $(WISPSRC)
	@echo ">>>> WISPKITDIR = " $(WISPKITDIR)
	@echo ">>>> KCSIKITDIR = " $(KCSIKITDIR)
	@echo ">>>> EDEKITDIR  = " $(EDEKITDIR)
	@echo ">>>> ACUDIR     = " $(ACUDIR)
	@echo ">>>> CD         = " 
	@CD
	@echo ">>>>"


prepare:  header prep_wisp prep_kcsi prep_ede

prep_wisp: clean_wisp
	mkdir $(BLDDIR_WISP)
	copy $(ACUDIR)\lib\* $(BLDDIR_WISP)
	copy $(WISPKITDIR)\lib\wisp.lib $(BLDDIR_WISP)
	copy $(WISPKITDIR)\lib\video.lib $(BLDDIR_WISP)
	copy $(WISPKITDIR)\acu\wisp_sub85_inc.c $(BLDDIR_WISP)
	copy $(WISPKITDIR)\acu\wispicon.ico $(BLDDIR_WISP)
	copy $(WISPKITDIR)\acu\acu90\sub85.c $(BLDDIR_WISP)
	copy $(WISPKITDIR)\acu\acu90\wisp.rc $(BLDDIR_WISP)
	copy $(WISPKITDIR)\acu\acu90\wrundll.vcproj $(BLDDIR_WISP)
	copy $(WISPKITDIR)\acu\acu90\run32.vcproj $(BLDDIR_WISP)
	copy $(WISPKITDIR)\acu\acu90\crun32.vcproj $(BLDDIR_WISP)


prep_kcsi: clean_kcsi
	mkdir $(BLDDIR_KCSI)
	copy $(ACUDIR)\lib\* $(BLDDIR_KCSI)
	copy $(WISPKITDIR)\lib\wisp.lib $(BLDDIR_KCSI)
	copy $(WISPKITDIR)\lib\video.lib $(BLDDIR_KCSI)
	copy $(WISPKITDIR)\acu\wisp_sub85_inc.c $(BLDDIR_KCSI)
	copy $(WISPKITDIR)\acu\wispicon.ico $(BLDDIR_KCSI)
	copy $(WISPKITDIR)\acu\acu90\sub85.c $(BLDDIR_KCSI)
	copy $(WISPKITDIR)\acu\acu90\wisp.rc $(BLDDIR_KCSI)
	copy $(KCSIKITDIR)\kcsiacu.lib $(BLDDIR_KCSI)
	copy $(KCSIKITDIR)\kcsi_sub85_inc.c $(BLDDIR_KCSI)
	copy $(KCSIKITDIR)\acu90\wrundll.vcproj $(BLDDIR_KCSI)
	copy $(KCSIKITDIR)\acu90\crun32.vcproj $(BLDDIR_KCSI)

prep_ede: clean_ede
	mkdir $(BLDDIR_EDE)
	copy $(ACUDIR)\lib\* $(BLDDIR_EDE)
	copy $(WISPKITDIR)\lib\wisp.lib $(BLDDIR_EDE)
	copy $(WISPKITDIR)\lib\video.lib $(BLDDIR_EDE)
	copy $(WISPKITDIR)\acu\wisp_sub85_inc.c $(BLDDIR_EDE)
	copy $(WISPKITDIR)\acu\wispicon.ico $(BLDDIR_EDE)
	copy $(WISPKITDIR)\acu\acu90\sub85.c $(BLDDIR_EDE)
	copy $(WISPKITDIR)\acu\acu90\wisp.rc $(BLDDIR_EDE)
	copy $(EDEKITDIR)\ede.lib $(BLDDIR_EDE)
	copy $(EDEKITDIR)\acu90\sub.c $(BLDDIR_EDE)
	copy $(EDEKITDIR)\acu90\wrundll.vcproj $(BLDDIR_EDE)
	copy $(EDEKITDIR)\acu90\run32.vcproj $(BLDDIR_EDE)
	copy $(EDEKITDIR)\acu90\crun32.vcproj $(BLDDIR_EDE)

clean: clean_wisp clean_kcsi clean_ede

clean_wisp:
	-rmdir /Q /S $(BLDDIR_WISP)

clean_kcsi:
	-rmdir /Q /S $(BLDDIR_KCSI)

clean_ede:
	-rmdir /Q /S $(BLDDIR_EDE)
