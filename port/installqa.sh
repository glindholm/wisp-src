#!/bin/sh
#/************************************************************************/
#/*									 */
#/*		 Copyright (c) 1988, 1989, 1990, 1991, 1992		 */
#/*	 An unpublished work of International Digital Scientific Inc.	 */
#/*			    All rights reserved.			 */
#/*									 */
#/************************************************************************/
#
#
#	File:		installqa.sh
#
#	Function:	This script installs a QA version of wisp 
#			to $WISP/wisp.QA.
#
#	Desciption:	This routine "installs" the QA ship kit.
#			- remove old $WISP/wisp.QA
#			- create $WISP/wisp.QA
#			- copy $WISP/src/ship/wisp to $WISP/wisp.QA
#			- add EDE and the runtime systems
#
#	Warning:	The previous version of $WISP/wisp.QA will be
#			deleted.
#
#	History:	06/30/92	Written. GSL
#

if [ ! -d $WISP ]
then
	echo 'Directory $WISP not found'
	echo installqa.sh Aborting!
	exit
fi

echo
echo "Enter the WISP version number (e.g. 3319) ?"
read ANS
if [ "$ANS" = "" ]
then
	echo Version can not be blank.
	exit 1
fi
VER=$ANS
echo
echo Using Version=[$VER]

#
#	Define some variables
#
SHIP=$WISP/src/wisp$VER.ship
SHIPWISP=$SHIP/wisp.$VER
SHIPEDE=$SHIP/ede.$VER
WISPQA=$WISP/wisp.QA

if [ ! -d $SHIPWISP ]
then
	echo '$SHIPWISP' "=$SHIPWISP"
	echo '$SHIPWISP does not exist, run bldshipkit.sh'
	echo installqa.sh Aborting!
	exit
fi

if [ -d $WISPQA ]
then
	echo
	echo 'Removing OLD $WISP/wisp.QA'
	rm -r -f $WISPQA

	if [ -d $WISPQA ]
	then
		echo Unable to remove $WISPQA
		echo installqa.sh Aborting!
		exit
	fi
fi

echo
echo 'Creating $WISP/wisp.QA'
mkdir $WISPQA

if [ ! -d $WISPQA ]
then
	echo Unable to create $WISPQA
	echo installqa.sh Aborting!
	exit
fi

echo
cd $SHIPWISP
echo pwd=`pwd`

echo
echo "Copying $SHIPWISP"
echo "     to $WISPQA"
echo
find . -print | cpio -pvdum $WISPQA
echo
echo "Adding  $SHIPEDE"
echo "    to  $WISPQA/ede"
echo
mkdir $WISPQA/ede
cd $SHIPEDE
find . -print | cpio -pvdum $WISPQA/ede
cp $SHIPEDE/good     $WISPQA/bin
cp $SHIPEDE/libede.a $WISPQA/lib

echo
echo 'Adding ACUCOBOL runtime systems'
cp $SHIP/rts/wruncbl  $WISPQA/bin
cp $SHIP/rts/wruncble $WISPQA/bin
cp $ACUDIR/bin/runcbl.alc $WISPQA/bin/wruncbl.alc
cp $ACUDIR/bin/runcbl.alc $WISPQA/bin/wruncble.alc

echo
echo 'Adding Micro Focus runtime systems'
cp $WISP/src/mf/wrunmf  $WISPQA/bin
cp $WISP/src/mf/wrunmfe $WISPQA/bin

echo
echo 'Adding CRID for Acucobol'
CRIDSHIP=$WISP/src/kcsi/crid/cridacu*.ship
if [ -d $CRIDSHIP ]
then
	cd $WISPQA
	uncompress -c $CRIDSHIP/cridacu.*.tar.Z|tar -xvf -
	ln -s cridacu.* cridacu
	cp $WISP/src/kcsi/crid/wruncblk $WISPQA/bin
	cp $ACUDIR/bin/runcbl.alc $WISPQA/bin/wruncblk.alc
fi

echo
echo 'Adding CRID for Micro Focus'
CRIDSHIP=$WISP/src/kcsi/crid/cridmfx*.ship
if [ -d $CRIDSHIP ]
then
	cd $WISPQA
	uncompress -c $CRIDSHIP/cridmfx.*.tar.Z|tar -xvf -
	ln -s cridmfx.* cridmfx
fi

echo
echo 'Adding CREATE for Acucobol'
CREATESHIP=$WISP/src/kcsi/create/createacu*.ship
if [ -d $CREATESHIP ]
then
	cd $WISPQA
	uncompress -c $CREATESHIP/createacu.*.tar.Z|tar -xvf -
	ln -s createacu.* createacu
fi

echo
echo 'Adding CREATE for Micro Focus'
CREATESHIP=$WISP/src/kcsi/create/createmfx*.ship
if [ -d $CREATESHIP ]
then
	cd $WISPQA
	uncompress -c $CREATESHIP/createmfx.*.tar.Z|tar -xvf -
	ln -s createmfx.* createmfx
fi

echo
echo '*** DONE ***'
