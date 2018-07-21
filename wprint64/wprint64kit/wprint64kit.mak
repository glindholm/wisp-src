# wprint64kit.mak
#
BASEDIR=C:\dev\wisp\src
KITDIR=C:\dev\wisp\src\kit\wprint64_1.0


dist_list=	$(KITDIR) \
		$(KITDIR)\wprint64.exe	\
		$(KITDIR)\wprint64_readme.txt	\
		$(KITDIR)\wprint64_usage.txt	\
		$(KITDIR)\wprint64_redist.txt	\
		$(KITDIR)\Microsoft.VC80.CRT	

#
#	targets
#
default: 	kit

clean: 
	-rmdir /Q /S $(KITDIR)

kit: clean $(dist_list)

$(KITDIR):
	mkdir $@
	
$(KITDIR)\wprint64.exe	\
:	$(BASEDIR)\bin\$(@F)
	copy $** $@
	
$(KITDIR)\wprint64_readme.txt	\
$(KITDIR)\wprint64_usage.txt	\
$(KITDIR)\wprint64_redist.txt	\
:	$(BASEDIR)\wprint64\$(@F)
	copy $** $@

$(KITDIR)\Microsoft.VC80.CRT \
:	$(BASEDIR)\nt\VS2005\amd64\$(@F)
	xcopy /S /I /Q  $** $@

