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
#	File:		blduniship.sh
#
#	Function:	This script builds the UNIQUE ship kit.
#
#	Desciption:	This routine is run after UNIQUE has been built
#			on the machine.  It builds the shipping kit 
#                       under the $UNIQUE directory and moves the
#			needed components into it.  The name of the
#                       kit will be $UNIQUE/uniXXX.ship where XXX is the
#			current version of UniQue.  The current version
#			number is substituted into this script at the
#			time that the shipping kit is built.  (Substitution
#			is done by the bldunisrc.sh script.)
#
#	Input:		$UNIQUE		The UNIQUE base directory.
#			$UNIQUE/src/...	The ported UniQue.
#
#	Output:		$UNIQUE/uniXXX.ship
#					The ship kit.
#
#	History:	12/28/93	Created by JEC using bldshipkit.sh as
#					starting point
#
#			7/7/94		Modified to be be run from the WISP
#					build proceedures. GSL


#
#	The UNIQUE version is set by modifying the following line or
#	setting the environment variable UNIQUE_VERSION.
#
VER=VERSION

if [ "$VER" = "VERSION" ]
then
	if [ $UNIQUE_VERSION ]
	then
		VER=$UNIQUE_VERSION
	else
		echo
		echo UNIQUE_VERSION is not set!
		echo blduniship.sh ABORTING!
		exit
	fi
fi

if [ "$UNIQUE" = "" ]
then
	if [ $WISP ]
	then
		UNIQUE=$WISP
	else
		echo
		echo Variable \$UNIQUE is not set!
		echo blduniship.sh ABORTING!
		exit
	fi
fi

if [ ! -d $UNIQUE/src ]
then
	echo
	echo Directory \$UNIQUE/src does not exist!
	echo blduniship.sh ABORTING!
	exit
fi

#
#	Define some variables
#
DISTDIR=unique.$VER
SHIPDIR=$UNIQUE/uniq$VER.ship
SHIP=$SHIPDIR/$DISTDIR
PQ=$UNIQUE/src/unique

#
#	Create all the SHIP KIT directories
#
if [ -d $SHIPDIR ]
then
	echo
	echo Removing old $SHIPDIR
	rm -r $SHIPDIR

	if [ -d $SHIPDIR ]
	then
		echo Unable to remove $SHIPDIR
		echo blduniship.sh Aborting!
		exit
	fi
fi

echo Creating $SHIPDIR
mkdir $SHIPDIR
mkdir $SHIP

#
#	Copy all the files into the ship kit
#

echo Loading $SHIP
cp $PQ/udaemon 		$SHIP
cp $PQ/ulp		$SHIP
cp $PQ/unique    	$SHIP
cp $PQ/ushut 		$SHIP
cp $PQ/ulicense 	$SHIP
cp $PQ/classdef.sam 	$SHIP
cp $PQ/formdef.sam 	$SHIP
cp $PQ/prtdef.sam 	$SHIP
cp $PQ/ulpdef.sam 	$SHIP
cp $PQ/ulpremote.sh 	$SHIP
cp $PQ/lpremote.sh 	$SHIP
cp $PQ/demoinstall.sh 	$SHIP
cp $PQ/uconfig.sh 	$SHIP
cp $PQ/uni$VER.rel   	$SHIP
cp $PQ/wispuniq.lis 	$SHIP

echo
echo UniQue Version `echo $VER|cut -c1`.`echo $VER|cut -c2-3` Shipping directory created.
cd $SHIPDIR
chmod a+rx $SHIP/*.sh
find $DISTDIR -print|cpio -ocv|compress -c >$DISTDIR.cpio.Z
echo
echo Please remember to copy compressed cpio archive to UniQue shipping
echo kit storage area on zaphod.
echo
