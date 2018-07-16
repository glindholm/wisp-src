#!/bin/sh
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
echo "VOLIN  $WISP/src/testacu/volin"	>> $WISPCONFIG/LGMAP
echo "VOLOUT $WISP/src/testacu/volout"	>> $WISPCONFIG/LGMAP
echo "VOLRUN $WISP/src/testacu/volrun"	>> $WISPCONFIG/LGMAP
echo "VOLSPL $WISP/src/testacu/volspl"	>> $WISPCONFIG/LGMAP
echo "VOLWRK $WISP/src/testacu/volwrk"	>> $WISPCONFIG/LGMAP


# Create wrunconfig
echo "cobol=ACU"			>  $WISPCONFIG/wrunconfig
echo "options=-b +e acuerr.log"		>> $WISPCONFIG/wrunconfig
echo "runcbl=$WISPDIR/cridacu/wruncblk"	>> $WISPCONFIG/wrunconfig

echo Loading $WISPCONFIG/videocap
cp $WISPDIR/config/videocap/* $WISPCONFIG/videocap

$WISPDIR/bin/wsysconf


echo FILE-STATUS-CODE 74 		                            > $WISPCONFIG/ACUCONFIG
echo CODE-PREFIX $WISP/src/testacu $WISPDIR/acu $WISPDIR/cridacu . >> $WISPCONFIG/ACUCONFIG
echo "#V-VERSION 3"			                           >> $WISPCONFIG/ACUCONFIG
echo ALTVARV3-VERSION 3			                           >> $WISPCONFIG/ACUCONFIG
echo ALTVARV2-VERSION 2			                           >> $WISPCONFIG/ACUCONFIG
echo KEYSTROKE EXCEPTION=33 5		         >> $WISPCONFIG/ACUCONFIG
echo KEYSTROKE EXCEPTION=34 6		         >> $WISPCONFIG/ACUCONFIG
echo KEYSTROKE EXCEPTION=34 27		         >> $WISPCONFIG/ACUCONFIG
echo SCREEN PROMPT=* PROMPT-ALL=YES	         >> $WISPCONFIG/ACUCONFIG

echo
echo The '$WISPCONFIG' $WISPCONFIG directory has been built.
echo
echo