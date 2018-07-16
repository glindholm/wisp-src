#
# makefile for WISP for NT/95 utilities DISPRINT.
#

WISPTRAN=wisp.exe
CCBL=ccbl32.exe

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

acu:	disprint.wcb
	$(WISPTRAN) -VACU disprint.wcb
	$(CCBL) -da4 -o disprint disprint.cob

acn:	disprint.wcb
	$(WISPTRAN) -VACN disprint.wcb
	$(CCBL) -da4 -o disprint disprint.cob

mf:	disprint.wcb
	$(WISPTRAN) -VMF disprint.wcb
	cob.exe -i disprint.cob

