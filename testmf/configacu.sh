#!/bin/sh
#/************************************************************************/
#/*									 */
#/*		 Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993	 */
#/*	 An unpublished work of International Digital Scientific Inc.	 */
#/*			    All rights reserved.			 */
#/*									 */
#/************************************************************************/
#
#
#	File:		configacu.sh
#
#	Function:	This script builds the testacu $WISPCONFIG.
#
#	Desciption:	This routine is run after WISP has been built
#			on the machine.  It builds the $WISPCONFIG
#			directory for testing ACUCOBOL.
#			
#				$WISP/src/testacu/config
#
#	Input:		$WISP		The WISP base directory.
#			$WISP/src/...	The ported WISP.
#
#	Output:		$WISP/src/testacu/config/...
#					The WISP config directory.
#
#	History:	02/18/93	Written by GSL
#

SCRIPT=configacu.sh

if [ x$WISP = x ]
then
	echo
	echo Variable \$WISP is not set!
	echo $SCRIPT ABORTING!
	exit
fi

TESTDIR=$WISP/src/testacu
if [ ! -d $TESTDIR ]
then
	echo
	echo Directory $TESTDIR does not exist!
	echo $SCRIPT ABORTING!
	exit
fi

#
#	Define some variables
#
SHIP=$WISP/src/ship
WU=$WISP/src/wisputils
PQ=$WISP/src/printq
PT=$WISP/src/proctran
VT=$WISP/src/videotest
VC=$WISP/src/videocap
WT=$WISP/src/wisptran
WL=$WISP/src/wisplib
VL=$WISP/src/videolib
ETC=$WISP/src/etc
PORT=$WISP/src/port
WACU=$WISP/src/acu
WMF=$WISP/src/mf
EDE=$WISP/src/ede

WISPCONFIG=$TESTDIR/config
export WISPCONFIG

#
#	Create all the $WISPCONFIG directories
#
if [ -d $WISPCONFIG ]
then
	echo
	echo Directory $WISPCONFIG already exists
	echo $SCRIPT ABORTING!
	exit
fi

echo Creating $WISPCONFIG
mkdir $WISPCONFIG
mkdir $WISPCONFIG/videocap
if [ ! -d $WISPCONFIG ]
then
	echo
	echo Unable to create $WISPCONFIG
	echo $SCRIPT ABORTING!
	exit
fi

echo Loading $WISPCONFIG
cp $ETC/forms		$WISPCONFIG/FORMS
cp $ETC/lpmap		$WISPCONFIG/LPMAP
cp $ETC/prmap		$WISPCONFIG/PRMAP
cp $ETC/scmap		$WISPCONFIG/SCMAP
cp $ETC/wispmsg.dat	$WISPCONFIG
cp $ETC/wispmsg.txt	$WISPCONFIG

cat $ETC/options.dat |sed "s|#IDSIPRINTON|IDSIPRINTON|"> $WISPCONFIG/OPTIONS
cat $TESTDIR/lgmap.acu |sed "s|_WISP_|$WISP|g"> $WISPCONFIG/LGMAP
cp $TESTDIR/wrunconfig.acu 	$WISPCONFIG/wrunconfig

echo Loading $WISPCONFIG/videocap
for org in $VC/*.vcap
do
	new=`basename $org .vcap`
	cp $org $WISPCONFIG/videocap/$new
done


$WU/wsysconf

cp $ACUDIR/cblconfig $WISPCONFIG/ACUCONFIG

echo
echo The '$WISPCONFIG' $WISPCONFIG directory has been built.
echo The following are still needed:
echo       '$WISPCONFIG/ACUCONFIG'  - needs modification
echo
echo
