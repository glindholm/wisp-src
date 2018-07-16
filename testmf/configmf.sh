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
#	File:		configmf.sh
#
#	Function:	This script builds the testmf $WISPCONFIG.
#
#	Desciption:	This routine is run after WISP has been built
#			on the machine.  It builds the $WISPCONFIG
#			directory for testing Micro Focus COBOL.
#			
#				$WISP/src/testmf/config
#
#	Input:		$WISP		The WISP base directory.
#			$WISP/src/...	The ported WISP.
#
#	Output:		$WISP/src/testmf/config/...
#					The WISP config directory.
#
#	History:	02/18/93	Written by GSL
#

SCRIPT=configmf.sh

if [ x$WISP = x ]
then
	echo
	echo Variable \$WISP is not set!
	echo $SCRIPT ABORTING!
	exit
fi

TESTDIR=$WISP/src/testmf
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
cp $ETC/CHARMAP		$WISPCONFIG
cp $ETC/FORMS		$WISPCONFIG
cp $ETC/LPMAP		$WISPCONFIG
cp $ETC/PRMAP		$WISPCONFIG
cp $ETC/SCMAP		$WISPCONFIG
cp $ETC/W4WMAP		$WISPCONFIG
cp $ETC/wispmsg.dat	$WISPCONFIG
cp $ETC/wispmsg.txt	$WISPCONFIG
cp ../wproc/wproc.msg	$WISPCONFIG

cat $ETC/OPTIONS |sed "s|#PQUNIQUE|PQUNIQUE|"> $WISPCONFIG/OPTIONS
cat $TESTDIR/lgmap.mf | sed "s|_WISP_|$WISP|g" > $WISPCONFIG/LGMAP
cp $TESTDIR/wrunconfig.mf $WISPCONFIG/wrunconfig

echo Loading $WISPCONFIG/videocap
for org in $VC/*.vcap
do
	new=`basename $org .vcap`
	cp $org $WISPCONFIG/videocap/$new
done


$WU/wsysconf

echo
echo The '$WISPCONFIG' $WISPCONFIG directory has been built.
echo
echo
