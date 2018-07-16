#
#	Copyright (c) 1988-1996 NeoMedia Migrations, All rights reserved.
#	$Id:$
#
#
#
#	File:		wisptran_test.mak
#
#	Project:	wisptran
#
#	RCS:		$Source:$
#
#	Function:	The nmake makefile for wisptran tests
#
#			
#

.IGNORE:

default: all


WISPTRAN  = ..\bin\wisp.exe
WISPLANG  = ACU
WISPFLAGS = -V $(WISPLANG) 

COBOL    = c:\acucorp\acucbl520\acugt\bin\ccbl32.exe
COBFLAGS = -Da4 -Gd -Zi -Za -Te 800

COPY=copy

.wcb.cob:
	$(WISPTRAN) $(WISPFLAGS) $*.wcb

.wcb.acu:
	$(WISPTRAN) $(WISPFLAGS) $*.wcb
	$(COBOL) $(COBFLAGS) -o $*.acu $*.cob

.cob.acu:
	$(COBOL) $(COBFLAGS) -o $*.acu $*.cob

.SUFFIXES: .acu .cob .wcb


TEST_COBOL_WCBS= \
	t_accept.wcb \
	t_add.wcb \
	t_call.wcb \
	t_close.wcb \
	t_commit.wcb \
	t_delete.wcb \
	t_display.wcb \
	t_free.wcb \
	t_go.wcb \
	t_hold.wcb \
	t_if.wcb \
	t_merge.wcb \
	t_move.wcb \
	t_open.wcb \
	t_perform.wcb \
	t_progid.wcb \
	t_read.wcb \
	t_rewrite.wcb \
	t_search.wcb \
	t_set.wcb \
	t_sort.wcb \
	t_start.wcb \
	t_stop.wcb \
	t_string.wcb \
	t_write.wcb \
	test0000.wcb \
	test0001.wcb

TEST_COBOL_COBS= $(TEST_COBOL_WCBS:.wcb=.cob)
TEST_COBOL_OBJS= $(TEST_COBOL_WCBS:.wcb=.acu)


all:	header $(TEST_COBOL_OBJS) 
	@echo " "
	@echo "TEST is up-to-date"
	@echo " "

header:
	@echo " "
	@echo "Building TEST programs"
	@echo " "
	@echo "WISPTRAN  =" $(WISPTRAN)
	@echo "WISPFLAGS =" $(WISPFLAGS)
	@echo "COBOL     =" $(COBOL)
	@echo "COBFLAGS  =" $(COBFLAGS)
	@echo "PWD       ="
	@cd
	@echo " "


clean:
	@echo "Deleting old builds"
	-del /Q /F $(TEST_COBOL_OBJS)
	-del /Q /F *.cob *.acu *.cpy

# Recreate the .cob files if $(WISPTRAN) is newer
$(TEST_COBOL_COBS): $(WISPTRAN)

#
#==============================================================
#



#
#==============================================================
#
