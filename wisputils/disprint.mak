#
# makefile for WISP for NT/95 utilities DISPRINT.
#

WISPDIR=C:\WISP
WISPTRAN=$(WISPDIR)\bin\wisp.exe

default:
	@echo '# You must specify a target "acu" or "mf"'
	@echo '#'
	@echo '# Usage:  nmake /f disprint.mak [target]'
	@echo '#'

acu:	disprint.wcb
	$(WISPTRAN) -VACU disprint.wcb
	ccbl32.exe -da4 -o disprint disprint.cob

mf:	disprint.wcb
	$(WISPTRAN) -VMF disprint.wcb
	cob.exe -i disprint.cob

