#/************************************************************************/
#/*									 */
#/*		 Copyright (c) 1988, 1989, 1990, 1991, 1992		 */
#/*	 An unpublished work of International Digital Scientific Inc.	 */
#/*			    All rights reserved.			 */
#/*									 */
#/************************************************************************/
#
#
#	File:		wrunmfx.umf
#
#			**** The IDSI ONLY version ****
#
#	Function:	The UNIX makefile for building the Micro Focus
#			COBOL/2  runtime systems that include the WISP 
#			runtime routines.
#
#	Description:	This makefile can generate the following versions
#			of the runtime system.
#
#			wrunmf		The standard runtime
#
#			wrunmfe		The EDE version of the runtime.
#
#	Instructions:	To create a runtime system follow the
#			instructions in Appendix A of the WISP Manual
#			"Building a Micro Focus COBOL/2 runtime system".
#
#			To build wrunmf:
#				$ make -f wrunmfx.umf
#
#			To build wrunmfe:
#				$ make -f wrunmfx.umf wrunmfe
#
#			To build the SUBS only:
#
#				$ make -f wrunmfx.umf subsonly
#
#	History:	06/09/92	Written. GSL
#			06/12/92	Added "subsonly". GSL
#

#
#	Leave CDEBUG blank - it will be filled in manually on the build
#
CDEBUG=

include $(WISP)/src/port/make.include

#	wispmf.o	- Micro Focus specific WISP routines.
#	wrunmf.c	- WISP routine list.
SUBS=wrunmf.o wispmf.o

LIBPATH=$(STDLIB)
LIBPATHS=$(LIBPATH)/libwisp.a $(LIBPATH)/libvideo.a
LIBPATHDS=$(LIBPATH)/libwispd.a $(LIBPATH)/libvideod.a
LIBS=-lwisp -lvideo -lm $(LIBCURSES)
LIBDS=-lwispd -lvideod -lm $(LIBCURSES)

all:	setup wrunmf
	@echo ">>> wrunmf  is up to date <<<"

both:	setup wrunmf wrunmfe
	@echo ">>> wrunmf  is up to date <<<"
	@echo ">>> wrunmfe is up to date <<<"

setup:
	@echo ">>> BUILDING MICRO FOCUS RUNTIME <<<"
	@echo "WISP    = " $(WISP)
	@echo "COBDIR  = " $(COBDIR)

subsonly:	$(SUBS)

wrunmf: $(SUBS) $(LIBPATHS)
	cob -xe "" -o $@ $(SUBS) -L $(LIBPATH) $(LIBS)

wrunmfd: $(SUBS) $(LIBPATHDS)
	cob -g -xe "" -o $@ $(SUBS) -L $(LIBPATH) $(LIBDS)

wrunmfe: $(SUBS) $(LIBPATHS) $(LIBPATH)/libede.a
	cob -xe "" -o $@ $(SUBS) -L $(LIBPATH) -lede $(LIBS)

