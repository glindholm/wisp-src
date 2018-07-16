# Makefile to create new version of "runcbl" or "acusql"
# based on changes to "sub.c"
#
# To relink the runtime, run: make runcbl
# To relink the ESQL pre-compiler, run: make acusql
#
# Distributed with ACUCOBOL-GT version 6.2.0
# PMK: 147

CC = cc -xarch=v9
CFLAGS = -xO3 $(ACUSERVER_FLAGS) $(ACUCONNECT_FLAGS) $(ACUSQL_FLAGS) \
	$(EXTSM_FLAGS) $(XML_FLAGS)
LDFLAGS = -s
SHELL = /bin/sh

# If your ACUCOBOL libraries are in another directory, change the value
# of ACU_LIBDIR to point at them.
ACU_LIBDIR = .

#
# Socket configuration
#
SOCKET_LIBS = -lnsl -lsocket

#
# Thread library configuration
#
THREAD_LIB =

#
# Shared library configuration
#
SHARED_LIBS = -ldl

#
# Compression library configuration
#
COMPRESSION_LIB = $(ACU_LIBDIR)/libz.a

#
# What are our libraries
#
ACME_LIB = \
	$(ACU_LIBDIR)/libsocks.a \
	$(ACU_LIBDIR)/libmessage.a \
	$(ACU_LIBDIR)/libcfg.a \
	$(ACU_LIBDIR)/liblib.a \
	$(ACU_LIBDIR)/libstdlib.a \
	$(ACU_LIBDIR)/libmemory.a
CLIENT_LIB = $(ACU_LIBDIR)/libclnt.a
RUNTIME_LIB = $(ACU_LIBDIR)/libruncbl.a
TERMMGR_LIB = $(ACU_LIBDIR)/libacuterm.a
VISION_LIB = $(ACU_LIBDIR)/libvision.a


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
# CICS configuration
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


#
# File System Interface configuration
#

# These subs/libs are for any extra file systems you want to link in.
XML_FLAGS = -DUSE_XML=1
FSI_SUBS = axml.o
FSI_LIBS = $(EXTFH_LIB) $(ACU_LIBDIR)/libexpat.a


LIBS =  $(ACUSERVER_LIBS) \
	$(ACU_LIBDIR)/libacvt.a \
	$(ACU_LIBDIR)/libfsi.a \
	$(TERMMGR_LIB) \
	$(FSI_LIBS) \
	$(VISION_LIB) \
	$(ACUSQL_LIBS) \
	$(CICS_LIB) \
	$(MQSERIES_LIB) \
	$(EXTSM_LIB) \
	$(ACME_LIB)

SYS_LIBS = \
	$(SOCKET_LIBS) \
	$(SHARED_LIBS) \
	$(THREAD_LIB) \
	$(COMPRESSION_LIB) \
	-lm

SUBS = sub.o filetbl.o $(FSI_SUBS)

runcbl: amain.o $(SUBS)
	$(CC) $(LDFLAGS) -o runcbl amain.o $(SUBS) $(RUNTIME_LIB) \
		$(LIBS) $(SYS_LIBS)

ALLOCA_LIBS =
ALLOCA_CFLAGS =

PREPROC_LIBS =	$(ACU_LIBDIR)/libpreproc.a \
		$(ACUSQL_ODBC_LIB) \
		$(ACME_LIB)

acusql: $(PREPROC_LIBS)
	$(CC) $(LDFLAGS) $(ALLOCA_CFLAGS) -o acusql $(PREPROC_LIBS) \
		$(ALLOCA_LIBS) $(SYS_LIBS)

clean:
	-rm -f runcbl acusql sub.o filetbl.o

# object dependencies
sub.o: sub.c sub85.c config85.c direct.c Makefile
filetbl.o: Makefile
