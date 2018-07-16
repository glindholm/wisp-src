# $Id:$
#
#	/************************************************************************/
#	/*									*/
#	/*		 Copyright (c) 1988, 1989, 1990, 1991, 1992		*/
#	/*	 An unpublished work of International Digital Scientific Inc.	*/
#	/*			    All rights reserved.			*/
#	/*									*/
#	/************************************************************************/
#
#
#	File:		printq.umf
#
#	Function:	Generic makefile for the ilp library on all unix platforms.
#
# 	History:	
#			06/04/92	Changed to use make.include GSL
#

CDEBUG = -g
#CDEBUG = -O

LIB = libilp.a

include $(WISP)/src/port/make.include

.PRECIOUS:	$(LIB) 

#
#	$(PQFLAG)
#
# -DSOCKCODE to include socket code;	Do not use for SCO.
# -DMQCODE to include message q code;	Use this one for SCO.
#
#   If a system complains that it can't resolve select(),
#   add -DNO_SELECT to $(PQFLAG).  This is done in make.include.
#

CFLAGS = $(CFLAGS0) $(PQFLAG)

LIBS = $(LIBCFLAGS) -lwisp -lvideo $(LIBSOC)
LIBPATHS= $(STDLIB)/libwisp.a $(STDLIB)/libvideo.a

YFLAGS=-d


DAEMONOBJS = d_main.o d_comm.o d_misc.o d_unspool.o utils.o  iprintcap_y.o iprintcap_l.o formdef_y.o formdef_l.o \
classdef_y.o classdef_l.o decode.o jlist.o qconfig_y.o qconfig_l.o
ILPMANOBJS = man_main.o man_scr.o c_comm.o utils.o c2vwrap.o man_misc.o
ILPOBJS = ipr_main.o c_comm.o utils.o c2vwrap.o 

all: 	idaemon ilpman ilp ishut $(STDLIB)/$(LIB)


ilpman: $(ILPMANOBJS) $(LIBPATHS)
	$(CC) $(CFLAGS) -o ilpman $(ILPMANOBJS) $(LIBS)

ilp: 	$(ILPOBJS) $(LIBPATHS)
	$(CC) $(CFLAGS) -o ilp $(ILPOBJS) $(LIBS)

man_main.o: man_main.c manage.h c_comm.h defs.h
man_misc.o: man_misc.c manage.h c_comm.h defs.h
ilp_wisp.o: ilp_wisp.c c_comm.h defs.h
man_scr.o: man_scr.c manage.h defs.h
c_comm.o: c_comm.c  defs.h c_comm.h
utils.o: utils.c  defs.h 

idaemon: $(DAEMONOBJS) $(LIBPATHS)
	$(CC) $(CFLAGS) -o idaemon $(DAEMONOBJS) $(LIBS)

jlist.o: jlist.c jlist.h
d_main.o: d_main.c daemon.h defs.h
d_misc.o: d_misc.c daemon.h defs.h
d_comm.o: d_comm.c daemon.h defs.h
d_unspool.o: d_unspool.c daemon.h defs.h
decode.o: decode.c
c2vwrap.o: c2vwrap.c

y.tab.h: iprintcap.y
	yacc -d iprintcap.y

iprintcap_y.o: iprintcap.y
	yacc -d iprintcap.y
	sed 's/yy/ipcyy/g' <y.tab.c >iprintcap_y.c
	sed 's/yy/ipcyy/g' <y.tab.h >iprintcap_y.h
	$(CC) $(CFLAGS) -c iprintcap_y.c
	rm iprintcap_y.c y.tab.c 
	
iprintcap_l.o: iprintcap.l 
	lex iprintcap.l
	sed 's/yy/ipcyy/g' <lex.yy.c >iprintcap_l.c
	$(CC) $(CFLAGS) -c iprintcap_l.c
	rm lex.yy.c iprintcap_l.c y.tab.h

formdef_y.o: formdef.y
	yacc -d formdef.y
	sed 's/yy/formyy/g' <y.tab.c >formdef_y.c
	sed 's/yy/formyy/g' <y.tab.h >formdef_y.h
	$(CC) $(CFLAGS) -c formdef_y.c
	rm formdef_y.c y.tab.c 

formdef_l.o: formdef.l
	lex formdef.l
	sed 's/yy/formyy/g' <lex.yy.c >formdef_l.c
	$(CC) $(CFLAGS) -c formdef_l.c
	rm lex.yy.c formdef_l.c 

classdef_y.o: classdef.y
	yacc -d classdef.y
	sed 's/yy/clsyy/g' <y.tab.c >classdef_y.c
	sed 's/yy/clsyy/g' <y.tab.h >classdef_y.h
	$(CC) $(CFLAGS) -c classdef_y.c
	rm classdef_y.c y.tab.c 

classdef_l.o: classdef.l
	lex classdef.l
	sed 's/yy/clsyy/g' <lex.yy.c >classdef_l.c
	$(CC) $(CFLAGS) -c classdef_l.c
	rm lex.yy.c classdef_l.c 

qconfig_y.o: qconfig.y
	@echo
	@echo expect \"1 rules never reduced\" and \"conflicts: 2 shift/reduce\"
	@echo
	yacc -d qconfig.y
	sed 's/yy/qcfyy/g' <y.tab.c >qconfig_y.c
	sed 's/yy/qcfyy/g' <y.tab.h >qconfig_y.h
	$(CC) $(CFLAGS) -c qconfig_y.c
	rm qconfig_y.c y.tab.c 

qconfig_l.o: qconfig.l
	lex qconfig.l
	sed 's/yy/qcfyy/g' <lex.yy.c >qconfig_l.c
	$(CC) $(CFLAGS) -c qconfig_l.c
	rm lex.yy.c qconfig_l.c 

clean:
	rm -f core *.o *.~ daemon ipman

cnt:	
	rm -f y.tab.*
	wc *.c *.h *.l *.y makefile

cntd:
	rm -f y.tab.*
	wc d_main.c d_comm.c d_misc.c d_unspool.c utils.c  iprintcap.y iprintcap.l daemon.h defs.h


cntm:	
	wc man_main.c man_scr.c c_comm.c utils.c c2vwrap.c ipr_main.c c_comm.c manage.h c_comm.h  defs.h

ishut: 	shut.c utils.o
	$(CC) $(CFLAGS) -o ishut shut.c utils.o

$(LIB): $(LIB)(ilp_wisp.o) $(LIB)(utils.o) $(LIB)(c_comm.o) $(LIB)(man_misc.o) $(LIB)(man_scr.o) $(LIB)(c2vwrap.o)
	$(RANLIB) $(LIB)
	@echo lib $(LIB) is now up-to-date

$(LIB)(ilp_wisp.o): ilp_wisp.o
	ar rv $(LIB) ilp_wisp.o

$(LIB)(utils.o): utils.o
	ar rv $(LIB) utils.o

$(LIB)(c_comm.o): c_comm.o
	ar rv $(LIB) c_comm.o

$(LIB)(man_misc.o): man_misc.o
	ar rv $(LIB) man_misc.o

$(LIB)(man_scr.o): man_scr.o
	ar rv $(LIB) man_scr.o

$(LIB)(c2vwrap.o): c2vwrap.c
	ar rv $(LIB) c2vwrap.o

$(STDLIB)/$(LIB): $(LIB)
	cp $(LIB) $@

install: 
	cp idaemon ishut  /usr/lib
	cp ilp ilpman     /usr/bin
	chown root  /usr/lib/idaemon
	chgrp 0     /usr/lib/idaemon
	chmod u+s   /usr/lib/idaemon
	chmod a-w   /usr/lib/idaemon

	
