#
#	Copyright (c) Shell Stream Software LLC, All rights reserved.
#	$Id:$
#
# makefile for WISP for WIN32 utilities DISPRINT.
#
#ACUDIR=C:\Acucorp\ACUCBL610\ACUGT
WISPDIR=C:\WISP5001

WISPTRAN=$(WISPDIR)\bin\wisp.exe
WISPFLAGS= -u ACU50
COBOL=$(ACUDIR)\bin\ccbl32.exe
COBFLAGS=-Da4 -Gd -Za -C50 -Z50

default:
	@echo '#'
	@echo '#'
	@echo '# Usage: nmake /f disprint.umf {acu/acn}'
	@echo '#'
	@echo '# To make DISPRINT for Acucobol enter the command:'
	@echo '#     $$ nmake /f disprint.umf acu'
	@echo '#'
	@echo '# To make DISPRINT for Acucobol Native Screens enter the command:'
	@echo '#     $$ nmake /f disprint.umf acn'
	@echo '#'

acu:	DISPRINT.wcb
	$(WISPTRAN) -VACU $(WISPFLAGS) DISPRINT.wcb
	$(COBOL) $(COBFLAGS) -o DISPRINT.acu DISPRINT.cob

acn:	DISPRINT.wcb
	$(WISPTRAN) -VACN $(WISPFLAGS) DISPRINT.wcb
	$(COBOL) $(COBFLAGS) -o DISPRINT.acu DISPRINT.cob

