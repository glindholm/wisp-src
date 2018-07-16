#!/bin/sh
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

if [ x$WISPDIR = x ]
then
	echo
	echo Variable \$WISPDIR is not set!
	echo $SCRIPT ABORTING!
	exit
fi

if [ ! -d $WISPDIR ]
then
	echo
	echo Directory $WISPDIR does not exist!
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
cp $WISPDIR/config/CHARMAP	$WISPCONFIG
cp $WISPDIR/config/FORMS	$WISPCONFIG
cp $WISPDIR/config/LPMAP	$WISPCONFIG
cp $WISPDIR/config/PRMAP	$WISPCONFIG
cp $WISPDIR/config/SCMAP	$WISPCONFIG
cp $WISPDIR/config/W4WMAP	$WISPCONFIG
cp $WISPDIR/config/wispmsg.dat	$WISPCONFIG
cp $WISPDIR/config/wispmsg.txt	$WISPCONFIG
cp $WISPDIR/config/wproc.msg	$WISPCONFIG

cat $WISPDIR/config/OPTIONS |sed "s|#PQLP|PQLP|"> $WISPCONFIG/OPTIONS
echo "SOURCE $WISP/src"			>  $WISPCONFIG/LGMAP
echo "VOLIN  $WISP/src/testmf/volin"	>> $WISPCONFIG/LGMAP
echo "VOLOUT $WISP/src/testmf/volout"	>> $WISPCONFIG/LGMAP
echo "VOLRUN $WISP/src/testmf/volrun"	>> $WISPCONFIG/LGMAP
echo "VOLSPL $WISP/src/testmf/volspl"	>> $WISPCONFIG/LGMAP
echo "VOLWRK $WISP/src/testmf/volwrk"	>> $WISPCONFIG/LGMAP

# Create wrunconfig
echo "cobol=MF"				>  $WISPCONFIG/wrunconfig
echo "options="				>> $WISPCONFIG/wrunconfig
echo "runcbl=$WISPDIR/mf/wrunmf"	>> $WISPCONFIG/wrunconfig

echo Loading $WISPCONFIG/videocap
cp $WISPDIR/config/videocap/* $WISPCONFIG/videocap

$WISPDIR/bin/wsysconf

echo
echo The '$WISPCONFIG' $WISPCONFIG directory has been built.
echo
echo
