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

if [ -d $WISP/wisp.QA ]
then
	echo
	echo 'Removing OLD $WISP/wisp.QA'
	rm -r $WISP/wisp.QA

	if [ -d $WISP/wisp.QA ]
	then
		echo Unable to remove $WISP/wisp.QA
		echo installqa.sh Aborting!
		exit
	fi
fi

echo
echo 'Creating $WISP/wisp.QA'
mkdir $WISP/wisp.QA

if [ ! -d $WISP/wisp.QA ]
then
	echo Unable to create $WISP/wisp.QA
	echo installqa.sh Aborting!
	exit
fi

if [ ! -d $WISP/src/ship/wisp ]
then
	echo '$WISP/src/ship/wisp does not exist'
	echo installqa.sh Aborting!
	exit
fi

echo
cd $WISP/src/ship/wisp
echo pwd=`pwd`

echo 'Copying $WISP/src/ship/wisp to $WISP/wisp.QA'
find . -print | cpio -pcvdum $WISP/wisp.QA

echo
echo 'Adding EDE to $WISP/wisp.QA'
cd $WISP/src/ship
find ede -print | cpio -pcvdum $WISP/wisp.QA
cp $WISP/src/ship/ede/good     $WISP/wisp.QA/bin
cp $WISP/src/ship/ede/libede.a $WISP/wisp.QA/lib

echo
echo 'Adding ACUCOBOL runtime systems'
cp $WISP/src/ship/rts/wruncbl  $WISP/wisp.QA/bin
cp $WISP/src/ship/rts/wruncble $WISP/wisp.QA/bin

echo
echo 'Adding Micro Focus runtime systems'
cp $WISP/src/mf/wrunmf  $WISP/wisp.QA/bin
cp $WISP/src/mf/wrunmfe $WISP/wisp.QA/bin

echo
echo '*** DONE ***'
