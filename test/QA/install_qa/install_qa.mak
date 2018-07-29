# install_qa.mak
#
#WISPSRC=..\..\..\..
QA=$(WISPSRC)\test\QA
KIT=$(WISPSRC)\kit
LICENSE=$(WISPSRC)\test\machine\$(COMPUTERNAME)\license.txt 


default: prepare

header: $(WISPDIR) $(ACUDIR) 
	@echo ">>>> INSTALLING WISP KITS INTO QA"
	@echo ">>>>"
	@echo ">>>> WISPSRC = " $(WISPSRC)
	@echo ">>>> QA      = " $(QA)
	@echo ">>>> KIT     = " $(KIT)
	@echo ">>>> CD      = " 
	@CD
	@echo ">>>>"


prepare:  header copystuff

copystuff: clean
	xcopy /S /I /Q $(KIT)\wisp $(QA)\wisp
	xcopy /S /I /Q $(KIT)\kcsiacu $(QA)\kcsiacu
	xcopy /S /I /Q $(KIT)\ede $(QA)\ede
	copy $(LICENSE) $(QA)\wisp\license.txt

clean: 
	-rmdir /Q /S $(QA)\wisp $(QA)\kcsiacu $(QA)\ede
