#############################################################################
#	File:	Makefile_aix53sh_32.mak
#
#	Acucobol:  8.1.0 
#	Platform:  AIX 5.3 32-bit
#	WISP:      5.1.00
#
#	This Makefile builds the Shared library version of the Acucobol
#	runtime. It has been modified to include the WISP runtime routines.
#	
#	Instructions:
#
#	In these instructions these macros are used as follows:
#       ${ACUDIR}  - Acucobol-GT 8.1.x directory (e.g. /opt/acu)
#       ${WISPDIR} - WISP installation directory (e.g. /opt/wisp5100)
#
#	The build is performed in the ${ACUDIR}/lib directory. It is 
#       recommended you save a copy of the original directory.
#          $ cd $ACUDIR
#	   $ cp -r lib lib.original
#
#	1) Copy the following WISP files into the ${ACUDIR}/lib folder.
#
#	   ${WISPDIR}/lib/libwisp.a
#	   ${WISPDIR}/lib/video.a
#	   ${WISPDIR}/acu/wisp_sub85_inc.c
#	   ${WISPDIR}/acu/sub85_acu81.c
#	   ${WISPDIR}/acu/acu81/Makefile_aix53sh_32.mak
#
#	2) Replace the file sub85.c with sub85_acu81.c
#
#	   $ mv sub85.c sub85.c.original
#	   $ cp sub85_acu81.c sub85.c
#
#	3) Build the new libruncbl.so shared runtime file
#
#	   $ make -f Makefile_aix53sh_32.mak clean libruncbl.so
#	
#	4) Setup your configuration to use the new runtime.
#
#	   Add $ACUDIR/lib to the LIBPATH per the Acucobol setup
#	   instructions.
#
#	   $ export LIBPATH=$ACUDIR/lib:$LIBPATH
#
#	   Edit $WISPCONFIG/wrunconfig and set the path to $ACUDIR/bin/runcbl.
#	   You can rename runcbl if you prefer but you cannot rename the shared
#	   libary file libruncbl.so.
#
#	   Example $WISPCONFIG/wrunconfig:
#	   cobol=ACU
#	   options=-b +e acuerr.log
#	   runcbl=/opt/acu/bin/runcbl
#
#
#
##############################################################################

# Makefile to create new version of "runcbl" or "acusql"
# based on changes to "sub.c"
#
# To relink the runtime, run: make runcbl
# To relink the ESQL pre-compiler, run: make acusql
#
# Distributed with ACUCOBOL-GT version 8.1.0
# PMK: 125

WISP_LIBS = libwisp.a libvideo.a -lcurses
WISP_FLAGS = -Dunix
EXTLIBS = $(WISP_LIBS)

VERSPATHNAME = 810
ACUVERSPATH = /opt/acucorp/810
ACUPATH = /opt/acu
ACUSQL_MAIN_MODULE = 
JAVA_FLAG =
CC = cc -q32 -qlanglvl=extc89 -qarch=com -qmaxmem=16384 -bmaxdata:0x80000000 -D_THREAD_SAFE 
CFLAGS = -O2 \
	$(ACUSERVER_FLAGS) $(ACUCONNECT_FLAGS) $(ACUSQL_FLAGS) $(EXTSM_FLAGS) \
	$(JAVA_FLAG) $(XML_FLAGS) $(EXTFH_FLAGS) $(WISP_FLAGS)
EXTRA_LDFLAG = -brtl -bnoipath -blibpath:$(ACUVERSPATH)/lib:$(ACUPATH)/lib:.:/usr/lib:/lib $(JAVA_LINK_FLAGS)
EXEC_LDFLAG =
EXPORT_LDFLAG = -bexpall -bautoexp -bE:runcbl.exp
LDFLAGS = -s $(EXTRA_LDFLAG)
SHARED_FLAG = -G
SONAME_FLAG =
SHAREDLIB_LDFLAGS = $(SHARED_FLAG) $(SONAME_FLAG) $(LDFLAGS)
SHARED_LIB_EXT = so
SYS_C_LIBS = -lc
NO_UNDEFS_LDFLAGS = -bernotok -brtllib
LIBRUNCBL_LDFLAGS =
# A sed script necessary for linking
LIB_SED = sed -e 's/\([\.\/a-z]*\)\/lib\([a-z]*\)\.$(SHARED_LIB_EXT)/-L \1 -l\2/g'
SHELL = /bin/sh

# If your ACUCOBOL libraries are in another directory, change the value
# of ACU_LIBDIR to point at them.
ACU_LIBDIR = .

# To use C$JAVA on HP-UX, you may need to relink the runtime with these lines
# uncommented.  Make sure the directories are correct for your machine.
# Check to see if JAVA_HOME is already defined, and also check for the
# appropriate directory for libjvm.sl (PA_RISC2.0 or PA_RISC2.0W)
# JAVA_HOME = /opt/java
# JAVA_LINK_FLAGS = -mt -L $(JAVA_HOME)/jre/lib/PA_RISC2.0W/server -ljvm

#
# Socket configuration
#
SOCKET_LIBS =

#
# Thread library configuration
#
THREAD_LIB =

#
# Shared library configuration
#
SHARED_LIBS =

#
# Compression library configuration
#
COMPRESSION_LIB = $(ACU_LIBDIR)/libz.a

#
# What are our libraries
#
ACME_LIB = $(ACU_LIBDIR)/libacme.$(SHARED_LIB_EXT)
CLIENT_LIB = 
REGEX_LIB = $(ACU_LIBDIR)/libaregex.$(SHARED_LIB_EXT)
TERMMGR_LIB = $(ACU_LIBDIR)/libacuterm.$(SHARED_LIB_EXT)
VISION_LIB = $(ACU_LIBDIR)/libvision.$(SHARED_LIB_EXT)
RUNTIME_LIBS = $(ACU_LIBDIR)/libruncbl.a


#
# AcuServer configuration
#

# Use the following two lines for an AcuServer-DISABLED runtime:
#ACUSERVER_FLAGS = -DNO_CLIENT=1
#ACUSERVER_LIBS = # nothing - no networking libraries are necessary

# Use the following two lines for an AcuServer-ENABLED runtime:
ACUSERVER_FLAGS =
ACUSERVER_LIBS = $(CLIENT_LIB)


#
# AcuConnect configuration
#

# Use the following line for an AcuConnect-DISABLED runtime:
#ACUCONNECT_FLAGS = -DNO_ACUCONNECT=1

# Use the following line for an AcuConnect-ENABLED runtime:
ACUCONNECT_FLAGS =


#
# CICS External Call Interface configuration
#

# Enter the name of your CICS client library below if you want to relink an
# CICS-ENABLED runtime:
CICS_LIB_USE =

# Use the following two lines for an CICS-DISABLED runtime:
CICS_FLAGS = -DNO_CICS=1
CICS_LIB = # nothing - no cics libraries are necessary

# Use the following two lines for an CICS-ENABLED runtime:
#CICS_FLAGS = -DNO_CICS=0
#CICS_LIB = $(CICS_LIB_USE)

#
# MQSERIES configuration
#

# Enter the name of your MQ Series client library below if you want to relink an
# MQSERIES-ENABLED runtime:
MQSERIES_LIB_USE =

# Use the following two lines for an MQSERIES-DISABLED runtime:
MQSERIES_FLAGS = -DNO_MQSERIES=1
MQSERIES_LIB = # nothing - no mqseries libraries are necessary

# Use the following two lines for an MQSERIES-ENABLED runtime:
#MQSERIES_FLAGS = -DNO_MQSERIES=0
#MQSERIES_LIB = $(MQSERIES_LIB_USE)


#
# AcuSQL configuration
#

# Enter the name of your ODBC API library below if you want to relink an
# AcuSQL-ENABLED runtime or the AcuSQL precompiler:
ACUSQL_ODBC_LIB =

# Use the following two lines for an AcuSQL-DISABLED runtime:
ACUSQL_FLAGS = -DNO_ACUSQL=1
ACUSQL_LIBS = # nothing - no acusql runtime libraries are necessary

# Use the following two lines for an AcuSQL-ENABLED runtime:
#ACUSQL_FLAGS = -DNO_ACUSQL=0
#ACUSQL_LIBS = $(ACU_LIBDIR)/libesql.a $(ACUSQL_ODBC_LIB)


#
# EXTSM configuration
#

# To relink with an external SORT library:
#   * Uncomment the top set of lines.
#   * Comment out the second set of lines.
#   * Set the value of the EXTSM_LIB variable.
#   * Set the value of the EXTFH_LIB variable in the next section.  Use
#     libextfh.a to access Vision files.
#
# Examples:
#     EXTSM_LIB = -L/usr/local/syncsort/lib -lmfsyncsort -lsyncsort
#     EXTSM_LIB = -L/usr/local/cosort8/lib -lmfcosort -lcosort -lpthread -lrt \
#                 -lposix4

# Uncomment the following two lines for an EXTSM-ENABLED runtime:
#EXTSM_LIB = # !!! replace this comment with the external SORT library
#EXTSM_FLAGS = -DUSE_EXTSM=1

# Uncomment the following two lines for an EXTSM-DISABLED runtime:
EXTSM_LIB = # nothing - no EXTSM libraries are necessary
EXTSM_FLAGS = -DUSE_EXTSM=0


#
# EXTFH configuration
#

# To link with an external file handler library:
#   * Uncomment the following line.
#   * Set the value of EXTFH_LIB variable to point to the library providing
#     the EXTFH function.  The default value, libextfh.a, is a library which
#     provides limited access to Vision via an EXTFH interface.  This library
#     is currently only intended for use by an EXTSM function.  If you have
#     another EXTFH library, substitute its file name below.

#EXTFH_LIB = libextfh.a

# If you are using the default library, libextfh.a, or if your EXTFH library
# provides a function named "EXTFH" then uncomment the following line.
#EXTFH_FLAGS = -DUSE_EXTFH=1


#
# File System Interface configuration
#

# These subs/libs are for any extra file systems you want to link in.
XML_FLAGS = -DUSE_XML=1
FSI_SUBS =
FSI_LIBS = $(EXTFH_LIB) $(ACU_LIBDIR)/libaxml.$(SHARED_LIB_EXT) $(ACU_LIBDIR)/libexpat.a


LIBS =  $(ACUSERVER_LIBS) \
	$(ACU_LIBDIR)/libacvt.a \
	$(ACU_LIBDIR)/libfsi.a \
	$(REGEX_LIB) \
	$(TERMMGR_LIB) \
	$(FSI_LIBS) \
	$(VISION_LIB) \
	$(ACUSQL_LIBS) \
	$(CICS_LIB) \
	$(MQSERIES_LIB) \
	$(EXTSM_LIB) \
	$(ACME_LIB) \
	$(COMPRESSION_LIB)

SYS_LIBS = \
	$(SOCKET_LIBS) \
	$(SHARED_LIBS) \
	$(THREAD_LIB) \
	-lm

SUBS = callc.o sub.o filetbl.o $(FSI_SUBS)

libruncbl.$(SHARED_LIB_EXT): libruncbl.a $(SUBS) $(RUNTIME_LIBS) runcbl.exp
	-rm -f libruncbl.$(SHARED_LIB_EXT)
	$(CC) $(SHAREDLIB_LDFLAGS) $(NO_UNDEFS_LDFLAGS) $(LIBRUNCBL_LDFLAGS) \
		$(EXPORT_LDFLAG) $(SUBS) libruncbl.a \
		-o libruncbl.$(SHARED_LIB_EXT) \
		`echo $(LIBS) | $(LIB_SED)` \
		$(SYS_LIBS) $(SYS_C_LIBS) $(EXTOBJS) $(EXTLIBS)

relinkrun:
	-rm -f libruncbl.$(SHARED_LIB_EXT)
	$(MAKE) libruncbl.$(SHARED_LIB_EXT)

runcbl: amain.o libruncbl.$(SHARED_LIB_EXT)
	$(CC) $(EXEC_LDFLAG) $(LDFLAGS) $(NO_UNDEFS_LDFLAGS) -o runcbl \
		amain.o -L . -lruncbl -lacme $(SYS_C_LIBS)


ALLOCA_LIBS =
ALLOCA_CFLAGS =

PREPROC_LIBS =	$(ACU_LIBDIR)/libpreproc.a \
		$(ACUSQL_ODBC_LIB) \
		$(ACME_LIB)

acusql: $(PREPROC_LIBS)
	$(CC) $(LDFLAGS) $(ALLOCA_CFLAGS) -o acusql \
		$(ACUSQL_MAIN_MODULE) \
		`echo $(PREPROC_LIBS) | $(LIB_SED)` \
		 $(COMPRESSION_LIB) \
		 $(ALLOCA_LIBS) $(SYS_LIBS) $(EXTOBJS) $(EXTLIBS)


runcbl.exp:
	echo Amain > runcbl.exp
	echo aculongjmp >> runcbl.exp
	echo acusavenv >> runcbl.exp
	echo acu_abend >> runcbl.exp
	echo acu_cancel >> runcbl.exp
	echo acu_cancel_all >> runcbl.exp
	echo acu_cobol >> runcbl.exp
	echo acu_convert >> runcbl.exp
	echo acu_initv >> runcbl.exp
	echo acu_reg_sig_handlers >> runcbl.exp
	echo acu_register_sub >> runcbl.exp
	echo acu_runmain >> runcbl.exp
	echo acu_shutdown >> runcbl.exp
	echo acu_unload >> runcbl.exp
	echo acu_unload_all >> runcbl.exp
	echo acu_unreg_sig_handlers >> runcbl.exp
	echo Java_com_acucorp_acucobolgt_CVM_acu_1initv >> runcbl.exp
	echo Java_com_acucorp_acucobolgt_CVM_acu_1cobol >> runcbl.exp
	echo Java_com_acucorp_acucobolgt_CVM_acu_1cancel >> runcbl.exp
	echo Java_com_acucorp_acucobolgt_CVM_acu_1unload >> runcbl.exp
	echo Java_com_acucorp_acucobolgt_CVM_logParams >> runcbl.exp
	echo Java_com_acucorp_acucobolgt_CVM_LogNativeMessages >> runcbl.exp
	echo Java_com_acucorp_acucobolgt_CVM_acu_1cancel_1all >> runcbl.exp
	echo Java_com_acucorp_acucobolgt_CVM_acu_1unload_1all >> runcbl.exp
	echo Java_com_acucorp_acucobolgt_CVM_acu_1shutdown >> runcbl.exp
	echo Java_com_acucorp_acucobolgt_CVM_setupJVM >> runcbl.exp

# this target prints the libraries necessary to link with our EXTFH routine:
print-extfh-libs:
	@echo	$(ACU_LIBDIR)/libextfh.a \
		$(ACU_LIBDIR)/libfsi.a \
		$(VISION_LIB) \
		$(ACME_LIB) \
		$(COMPRESSION_LIB)

clean:
	-rm -f acusql callc.o filetbl.o sub.o
	-rm -f libruncbl.$(SHARED_LIB_EXT)

# object dependencies
callc.o: callc.c sub.h Makefile
filetbl.o: filetbl.c Makefile
sub.o: sub.c sub.h sub85.c config85.c direct.c Makefile
