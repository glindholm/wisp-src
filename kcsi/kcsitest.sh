#!/bin/sh
#/************************************************************************/
#/*                                                                      */
#/*              		Copyright (c) 1997        		 */
#/*      	An unpublished work of NeoMedia Technologies Inc.    	 */
#/*                         All rights reserved.                         */
#/*                                                                      */
#/************************************************************************/
#
#
#       File:           kcsitest.sh
#
#	TO EXECUTE:	$ kcsitest.sh >>kcsi.log 2>&1
#			Look at log and confirm file formats
#
#       Function:       This script builds the test environment
#			for KCSI and Micro Focsu COBOL with the
#			use of the extended file format EXTFH().
#
#       Desciption:     This routine is run after the $WISPCONFIG
#                       directory is built for testing Micro Focus
#			COBOL.  startup.mf must be executed and
#			make sure that samplemf.umf has also been
#			invoked.  Finally, make sure that QA0000
#			has been run to set up the usage constants.
#
#
#       Input:          ${WISPSRC}/...  The WISP source directory.
#
#       Output:         ${WISPSRC}/testmf/volin/kcsictl/...
#                               The CONTROL data file directory.
#			${WISPSRC}/testmf/volin/libin/...
#				Data files for testing KCSI.
#			${WISPSRC}/testmf/volrun/librun/...
#				Create data file for testing KCSI.
#
#       History:        11/03/97        Written by SMC
#

SCRIPT=kcsitest.sh

if [ x${WISPSRC} = x ]
then
        echo
        echo Variable \${WISPSRC} is not set!
        echo $SCRIPT ABORTING!
        exit
fi

TESTDIR=${WISPSRC}/testmf
if [ ! -d $TESTDIR ]
then
        echo
        echo Directory $TESTDIR does not exist!
        echo $SCRIPT ABORTING!
        exit
fi

#
#       Define some variables
#
KCSISRC=${WISPSRC}/kcsi/crid
KCSICTL=${WISPSRC}/testmf/volin/kcsictl
KCSIDTA=${WISPSRC}/testmf/volin/libin
KCSIPRG=${WISPSRC}/testmf/volrun/librun

echo Creating $KCSICTL
mkdir $KCSICTL
if [ ! -d $KCSICTL ]
then
        echo
        echo Unable to create $KCSICTL
        echo $SCRIPT ABORTING!
        exit
fi

chmod u+rw $KCSICTL
echo Loading $KCSICTL
cp $KCSISRC/ctlseq $KCSICTL
cp $KCSISRC/ctlseq.idx $KCSICTL
cp $KCSISRC/kcsictl.inf $KCSICTL

echo Loading $KCSIDTA
cp $KCSISRC/kcsiseq $KCSIDTA
cp $KCSISRC/kcsiidx.inf $KCSIDTA
cp $KCSISRC/kcsii3.inf $KCSIDTA
cp $KCSISRC/kcsiidx.i3 $KCSIDTA
cp $KCSISRC/kcsii4.inf $KCSIDTA
cp $KCSISRC/kcsiidx.i4 $KCSIDTA

echo Loading $KCSIPRG
cp $KCSISRC/tstextfh.wcb $KCSIPRG

echo Building $KCSIPRG/TSTEXTFH.int
cd $KCSIPRG
wac tstextfh .

echo Creating data files
#  Creates native index data file using kcsiseq 
wrun TSTEXTFH

cd $KCSIDTA
echo ""
echo "<<<<< VERIFY is native index file for input type >>>>>"
fhinfo -rv kcsiidx.inf
# Creates IDXFORMAT 3 data file
fhconvert -rv kcsiidx.i3
echo ""
echo "<<<<< VERIFY is IDXFORMAT 3 (I3) for input type >>>>>"
fhinfo -rv kcsii3.inf
# Creates IDXFORMAT 4 data file
fhconvert -rv kcsiidx.i4
echo ""
echo "<<<<< VERIFY is IDXFORMAT 4 (I4) for input type >>>>>"
fhinfo -rv kcsii4.inf

cd $KCSICTL
# Creates index CONTROL file using ctlseq
fhconvert -rv ctlseq.idx
echo ""
echo "<<<<< VERIFY is native index file for input type >>>>>"
fhinfo -rv kcsictl.inf

echo ""
echo Test KCSI setup complete
