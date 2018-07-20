# Makefile to create new version of "runcbl" or "acusql"
# based on changes to "sub.c"
#
# To relink the runtime, run: make runcbl
# To relink the ESQL pre-compiler, run: make acusql
#
# Distributed with ACUCOBOL-GT version 9.0.1
# PMK: 120

VERSPATHNAME = 901
ACUVERSPATH = /opt/acucorp/901
ACUPATH = /opt/acu
ACUSQL_MAIN_MODULE = 
JAVA_FLAG =
CC = cc -Ae +DSblended -Wl,+s -D_REENTRANT -D_LARGEFILE64_SOURCE +z
EXTRA_CFLAGS =
CFLAGS = +O2 \
	$(ACUSERVER_FLAGS) $(ACUCONNECT_FLAGS) $(ACUSQL_FLAGS) $(EXTSM_FLAGS) \
	$(JAVA_FLAG) $(XML_FLAGS) $(EXTFH_FLAGS) -DUSE_RMFM $(EXTRA_CFLAGS)
EXTRA_LDFLAG = -Wl,+b -Wl,$(ACUVERSPATH)/lib:$(ACUPATH)/lib:.:/usr/lib:/lib $(JAVA_LINK_FLAGS)
EXEC_LDFLAG = -Wl,-E
EXPORT_LDFLAG =
LDFLAGS = -s $(EXTRA_LDFLAG)
SHARED_FLAG = -b
SONAME_FLAG =
BITS =
SHAREDLIB_LDFLAGS = $(SHARED_FLAG) $(SONAME_FLAG) $(LDFLAGS)
SHARED_LIB_EXT = so
SYS_C_LIBS =
NO_UNDEFS_LDFLAGS = -Wl,+k
LIBRUNCBL_LDFLAGS = -u Amain -u gzdopen -u uncompress
# A sed script necessary for linking
LIB_SED = sed -e 's/\([\.\/a-z0-9]*\)\/lib\([a-z0-9-]*\)\.$(SHARED_LIB_EXT)/-L\1 -l\2/g'
SHELL = /bin/sh

# If your ACUCOBOL libraries are in another directory, change the value
# of ACU_LIBDIR to point at them.
ACU_LIBDIR = .

# To use C$JAVA on HP-UX, you may need to relink the runtime with these lines
# uncommented.  Make sure the directories are correct for your machine.
# Check to see if JAVA_HOME is already defined, and also check for the
# appropriate directory for libjvm.sl (PA_RISC2.0 or PA_RISC2.0W)
# JAVA_HOME = /opt/java
# JAVA_LINK_FLAGS = -mt -L$(JAVA_HOME)/jre/lib/PA_RISC2.0W/server -ljvm

#
# Socket configuration
#
SOCKET_LIBS =

#
# Thread library configuration
#
THREAD_LIB = -lpthread

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
ACME2_LIB = $(ACU_LIBDIR)/libacme2$(BITS).$(SHARED_LIB_EXT)
ACME_LIB = \
	$(ACU_LIBDIR)/libacme$(BITS).$(SHARED_LIB_EXT) \
	$(ACU_LIBDIR)/libcobacme$(BITS).$(SHARED_LIB_EXT)
CLIENT_LIB = 
REGEX_LIB = $(ACU_LIBDIR)/libaregex$(BITS).$(SHARED_LIB_EXT)
TERMMGR_LIB = $(ACU_LIBDIR)/libacuterm$(BITS).$(SHARED_LIB_EXT)
AXML_LIB = $(ACU_LIBDIR)/libaxml$(BITS).$(SHARED_LIB_EXT)
VISION_LIB = $(ACU_LIBDIR)/libvision$(BITS).$(SHARED_LIB_EXT)
ACVT_LIB = $(ACU_LIBDIR)/libacvt$(BITS).$(SHARED_LIB_EXT)
AFSI_LIB = $(ACU_LIBDIR)/libfsi.a
AFSI2_LIB = $(ACU_LIBDIR)/libfsi2$(BITS).$(SHARED_LIB_EXT)
EXPAT_LIB = $(ACU_LIBDIR)/libexpat.a
RUNTIME_LIBS = $(ACU_LIBDIR)/libruncbl$(BITS).a


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
FSI_LIBS = $(EXTFH_LIB) $(ACU_LIBDIR)/libexpat.a


LIBS =  $(ACUSERVER_LIBS) \
	$(ACVT_LIB) \
	$(AFSI_LIB) \
	$(AXML_LIB) \
	$(REGEX_LIB) \
	$(TERMMGR_LIB) \
	$(FSI_LIBS) \
	$(VISION_LIB) \
	$(ACUSQL_LIBS) \
	$(CICS_LIB) \
	$(MQSERIES_LIB) \
	$(EXTSM_LIB) \
	$(ACME2_LIB) \
	$(ACME_LIB) \
	$(COMPRESSION_LIB)

ACUUSER_LIBS = \
	$(TERMMGR_LIB) \
	cobaculibs$(BITS).$(SHARED_LIB_EXT) \
	libacme$(BITS).$(SHARED_LIB_EXT)

SYS_LIBS = \
	$(SOCKET_LIBS) \
	$(SHARED_LIBS) \
	$(THREAD_LIB) \
	-lm

SUBS = callc.o sub.o filetbl.o $(FSI_SUBS)

libruncbl$(BITS).$(SHARED_LIB_EXT): libruncbl$(BITS).a $(SUBS) $(RUNTIME_LIBS) \
		runcbl.exp
	-rm -f $@
	$(CC) $(SHAREDLIB_LDFLAGS) $(NO_UNDEFS_LDFLAGS) $(LIBRUNCBL_LDFLAGS) \
		$(EXPORT_LDFLAG) $(SUBS) libruncbl$(BITS).a -o $@ \
		`echo $(LIBS) | $(LIB_SED)` $(SYS_LIBS) $(SYS_C_LIBS) \
		$(EXTOBJS) $(EXTLIBS)

relinkrun:
	-rm -f libruncbl$(BITS).$(SHARED_LIB_EXT)
	$(MAKE) libruncbl$(BITS).$(SHARED_LIB_EXT)

runcbl: amain.o libruncbl$(BITS).$(SHARED_LIB_EXT)
	$(CC) $(EXEC_LDFLAG) $(LDFLAGS) $(NO_UNDEFS_LDFLAGS) -o $@ \
		amain.o -L. -lruncbl$(BITS) -lacme$(BITS) -lacme2$(BITS) \
		$(SYS_C_LIBS)

runcbl.exp:
	-rm -f $@
	echo Amain >> $@
	echo aculongjmp >> $@
	echo acusavenv >> $@
	echo acu_abend >> $@
	echo acu_cancel >> $@
	echo acu_cancel_all >> $@
	echo acu_cobol >> $@
	echo acu_convert >> $@
	echo acu_initv >> $@
	echo acu_reg_sig_handlers >> $@
	echo acu_register_sub >> $@
	echo acu_runmain >> $@
	echo acu_shutdown >> $@
	echo acu_unload >> $@
	echo acu_unload_all >> $@
	echo acu_unreg_sig_handlers >> $@
	echo bis_int_signal >> $@
	echo Java_com_acucorp_acucobolgt_CVM_acu_1initv >> $@
	echo Java_com_acucorp_acucobolgt_CVM_acu_1cobol >> $@
	echo Java_com_acucorp_acucobolgt_CVM_acu_1cancel >> $@
	echo Java_com_acucorp_acucobolgt_CVM_acu_1unload >> $@
	echo Java_com_acucorp_acucobolgt_CVM_logParams >> $@
	echo Java_com_acucorp_acucobolgt_CVM_LogNativeMessages >> $@
	echo Java_com_acucorp_acucobolgt_CVM_acu_1cancel_1all >> $@
	echo Java_com_acucorp_acucobolgt_CVM_acu_1unload_1all >> $@
	echo Java_com_acucorp_acucobolgt_CVM_acu_1shutdown >> $@
	echo Java_com_acucorp_acucobolgt_CVM_setupJVM >> $@

ALLOCA_LIBS =
ALLOCA_CFLAGS =

PREPROC_LIBS =	$(ACU_LIBDIR)/libpreproc.a \
		$(ACUSQL_ODBC_LIB) \
		$(ACME_LIB)

acusql: $(PREPROC_LIBS)
	$(CC) $(LDFLAGS) $(ALLOCA_CFLAGS) -o $@ $(ACUSQL_MAIN_MODULE) \
		`echo $(PREPROC_LIBS) | $(LIB_SED)` \
		 $(COMPRESSION_LIB) \
		 $(ALLOCA_LIBS) $(SYS_LIBS) $(EXTOBJS) $(EXTLIBS)

cobacuuser$(BITS).$(SHARED_LIB_EXT): msub.o acuuser.exp
	-rm -f $@
	$(CC) $(SHAREDLIB_LDFLAGS) $(NO_UNDEFS_LDFLAGS) $(LIBRUNCBL_LDFLAGS) \
		$(EXPORT_LDFLAG) msub.o -o $@ \
		`echo $(ACUUSER_LIBS) | $(LIB_SED)` $(SYS_LIBS)

acuuser.exp:
	echo MFACU_TABLES > $@

# this target prints the libraries necessary to link with our EXTFH routine:
print-extfh-libs:
	@echo	$(ACU_LIBDIR)/libextfh.a \
		$(AFSI_LIB) \
		$(VISION_LIB) \
		$(ACME_LIB) \
		$(COMPRESSION_LIB)

# This target prints the libraries necessary for some external file system
# projects.
TRI_LIBS =	$(CLIENT_LIB) \
		$(AFSI_LIB) \
		$(ACVT_LIB) \
		$(AXML_LIB) \
		$(REGEX_LIB)  \
		$(EXPAT_LIB) \
		$(VISION_LIB) \
		$(ACME2_LIB) \
		$(ACME_LIB) \
		$(COMPRESSION_LIB) \
		$(SYS_LIBS)

print-tri-libs:
	@echo $(TRI_LIBS) 

clean:
	-rm -f runcbl acusql callc.o filetbl.o sub.o msub.o
	-rm -f libruncbl$(BITS).$(SHARED_LIB_EXT)
	-rm -f cobacuuser$(BITS).$(SHARED_LIB_EXT)
	-rm -f runcbl.exp acuuser.exp

# object dependencies
callc.o: callc.c sub.h Makefile
filetbl.o: filetbl.c Makefile
sub.o: sub.c sub.h sub85.c config85.c direct.c Makefile
msub.o: sub.c sub.h sub85.c direct.c Makefile
	$(CC) -c $(CFLAGS) -DMELD_DLL -o msub.o sub.c
