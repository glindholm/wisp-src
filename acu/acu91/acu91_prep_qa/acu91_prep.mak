# acu91_prep.mak
#
ACUDIR=C:\data\Acucorp\ACUCBL910\ACUGT
WISPKITDIR=C:\dev\wisp\src\kit\wisp
KCSIKITDIR=C:\dev\wisp\src\kit\kcsiacu
EDEKITDIR=C:\dev\wisp\src\kit\ede
BLDDIR_WISP=acu_lib_wisp
BLDDIR_KCSI=acu_lib_kcsi
BLDDIR_EDE=acu_lib_ede

default: prepare

header: $(WISPKITDIR) $(ACUDIR) 
	@echo ">>>> PREPARING TO BUILD ACUCOBOL 9.1 RUNTIMES"
	@echo ">>>>"
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
	copy $(WISPKITDIR)\acu\acu91\sub85.c $(BLDDIR_WISP)
	copy $(WISPKITDIR)\acu\acu91\wisp.rc $(BLDDIR_WISP)
	copy $(WISPKITDIR)\acu\acu91\wrundll.vcxproj $(BLDDIR_WISP)
	copy $(WISPKITDIR)\acu\acu91\run32.vcxproj $(BLDDIR_WISP)
	copy $(WISPKITDIR)\acu\acu91\crun32.vcxproj $(BLDDIR_WISP)
	copy $(WISPKITDIR)\acu\acu91\wrun32.sln $(BLDDIR_WISP)
	copy $(WISPKITDIR)\acu\acu91\run32.sln $(BLDDIR_WISP)
	copy $(WISPKITDIR)\acu\acu91\crun32.sln $(BLDDIR_WISP)


prep_kcsi: clean_kcsi
	mkdir $(BLDDIR_KCSI)
	copy $(ACUDIR)\lib\* $(BLDDIR_KCSI)
	copy $(WISPKITDIR)\lib\wisp.lib $(BLDDIR_KCSI)
	copy $(WISPKITDIR)\lib\video.lib $(BLDDIR_KCSI)
	copy $(WISPKITDIR)\acu\wisp_sub85_inc.c $(BLDDIR_KCSI)
	copy $(WISPKITDIR)\acu\wispicon.ico $(BLDDIR_KCSI)
	copy $(WISPKITDIR)\acu\acu91\sub85.c $(BLDDIR_KCSI)
	copy $(WISPKITDIR)\acu\acu91\wisp.rc $(BLDDIR_KCSI)
	copy $(KCSIKITDIR)\kcsiacu.lib $(BLDDIR_KCSI)
	copy $(KCSIKITDIR)\kcsi_sub85_inc.c $(BLDDIR_KCSI)
	copy $(KCSIKITDIR)\acu91\wrundll.vcxproj $(BLDDIR_KCSI)
	copy $(KCSIKITDIR)\acu91\crun32.vcxproj $(BLDDIR_KCSI)
	copy $(KCSIKITDIR)\acu91\run32.vcxproj $(BLDDIR_KCSI)
	copy $(KCSIKITDIR)\acu91\wrun32.sln $(BLDDIR_KCSI)
	copy $(KCSIKITDIR)\acu91\crun32.sln $(BLDDIR_KCSI)
	copy $(KCSIKITDIR)\acu91\run32.sln $(BLDDIR_KCSI)

prep_ede: clean_ede
	mkdir $(BLDDIR_EDE)
	copy $(ACUDIR)\lib\* $(BLDDIR_EDE)
	copy $(WISPKITDIR)\lib\wisp.lib $(BLDDIR_EDE)
	copy $(WISPKITDIR)\lib\video.lib $(BLDDIR_EDE)
	copy $(WISPKITDIR)\acu\wisp_sub85_inc.c $(BLDDIR_EDE)
	copy $(WISPKITDIR)\acu\wispicon.ico $(BLDDIR_EDE)
	copy $(WISPKITDIR)\acu\acu91\sub85.c $(BLDDIR_EDE)
	copy $(WISPKITDIR)\acu\acu91\wisp.rc $(BLDDIR_EDE)
	copy $(EDEKITDIR)\ede.lib $(BLDDIR_EDE)
	copy $(EDEKITDIR)\acu91\sub.c $(BLDDIR_EDE)
	copy $(EDEKITDIR)\acu91\wrundll.vcxproj $(BLDDIR_EDE)
	copy $(EDEKITDIR)\acu91\run32.vcxproj $(BLDDIR_EDE)
	copy $(EDEKITDIR)\acu91\crun32.vcxproj $(BLDDIR_EDE)
	copy $(EDEKITDIR)\acu91\wrun32.sln $(BLDDIR_EDE)
	copy $(EDEKITDIR)\acu91\run32.sln $(BLDDIR_EDE)
	copy $(EDEKITDIR)\acu91\crun32.sln $(BLDDIR_EDE)

clean: clean_wisp clean_kcsi clean_ede

clean_wisp:
	-rmdir /Q /S $(BLDDIR_WISP)

clean_kcsi:
	-rmdir /Q /S $(BLDDIR_KCSI)

clean_ede:
	-rmdir /Q /S $(BLDDIR_EDE)
