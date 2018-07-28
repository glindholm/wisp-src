# wprint64kit.mak
#
#WISPSRC=..\..\..
KITDIR=$(WISPSRC)\kit\wprint64_1.0


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
:	$(WISPSRC)\bin\$(@F)
	copy $** $@
	
$(KITDIR)\wprint64_readme.txt	\
$(KITDIR)\wprint64_usage.txt	\
$(KITDIR)\wprint64_redist.txt	\
:	$(WISPSRC)\wprint64\$(@F)
	copy $** $@

$(KITDIR)\Microsoft.VC80.CRT \
:	$(WISPSRC)\nt\VS2005\amd64\$(@F)
	xcopy /S /I /Q  $** $@

