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
#	File:		bldshipkit.sh
#
#	Function:	This script builds the WISP ship kit.
#
#	Desciption:	This routine is run after WISP has been built
#			on the machine.  It builds the temporary shipping
#			kit area under the "src" directory and moves all
#			the needed components into it.  Once this temp
#			shipping kit has been tested it would be renamed
#			and moved out of the "src" directory to become
#			the real shipping kit.
#
#	Input:		$WISP		The WISP base directory.
#			$WISP/src/...	The ported WISP.
#
#	Output:		$WISP/src/ship/...
#					The temporary ship kit.
#
#	History:	06/09/92	Written by GSL
#			06/12/92	Add ilpremote/ilpsrv, wshell, make.include. GSL
#			06/30/92	Add wdiag. GSL
#			07/08/92	Add info messages. GSL
#			07/08/92	Add the loading of $SHIP/rts. GSL
#			03/11/93	Add VSEDIT. GSL
#			07/23/93	Removed CRID and added IVS. GSL
#

if [ x$WISP = x ]
then
	echo
	echo Variable \$WISP is not set!
	echo bldshipkit.sh ABORTING!
	exit
fi

if [ ! -d $WISP/src ]
then
	echo
	echo Directory \$WISP/src does not exist!
	echo bldshipkit.sh ABORTING!
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

#
#	Create all the SHIP KIT directories
#
if [ -d $SHIP ]
then
	echo
	echo 'Removing OLD $WISP/src/ship'
	rm -r $SHIP

	if [ -d $SHIP ]
	then
		echo Unable to remove $SHIP
		echo bldshipkit.sh Aborting!
		exit
	fi
fi

echo Creating $WISP/src/ship
mkdir $SHIP
mkdir $SHIP/wisp
mkdir $SHIP/wisp/bin
mkdir $SHIP/wisp/lib
mkdir $SHIP/wisp/etc
mkdir $SHIP/wisp/acu
mkdir $SHIP/wisp/mf
mkdir $SHIP/wisp/config
mkdir $SHIP/wisp/config/videocap
mkdir $SHIP/ede
mkdir $SHIP/ede/demo
mkdir $SHIP/rts
mkdir $SHIP/ivs

#
#	Copy all the files into the ship kit
#

echo Loading $SHIP/wisp/bin
cp $WU/bldmf		$SHIP/wisp/bin
cp $WU/display		$SHIP/wisp/bin
cp $PQ/idaemon		$SHIP/wisp/bin
cp $PQ/ilp		$SHIP/wisp/bin
cp $PQ/ilpman		$SHIP/wisp/bin
cp $PQ/ishut		$SHIP/wisp/bin
cp $WU/makemsg		$SHIP/wisp/bin
cp $PT/proctran		$SHIP/wisp/bin
cp $WU/selectpg		$SHIP/wisp/bin
cp $WU/vcapkeys		$SHIP/wisp/bin
cp $WU/viewkey		$SHIP/wisp/bin
cp $WISP/src/vsedit/vsedit $SHIP/wisp/bin
cp $WU/vsx		$SHIP/wisp/bin
cp $VT/vtest		$SHIP/wisp/bin
cp $WU/wcopy		$SHIP/wisp/bin
cp $WU/wdelay		$SHIP/wisp/bin
cp $WU/wdelwrk		$SHIP/wisp/bin
cp $WU/wdiag		$SHIP/wisp/bin
cp $WU/wexists		$SHIP/wisp/bin
cp $WU/wfind		$SHIP/wisp/bin
cp $WT/wisp		$SHIP/wisp/bin
cp $WU/wputparm		$SHIP/wisp/bin
cp $WU/wrename		$SHIP/wisp/bin
cp $WU/wretcode		$SHIP/wisp/bin
cp $WU/wrun		$SHIP/wisp/bin
cp $WU/wscratch		$SHIP/wisp/bin
cp $WU/wshell.sh	$SHIP/wisp/bin/wshell
cp $WU/wsort		$SHIP/wisp/bin
cp $WU/wsubmit		$SHIP/wisp/bin
cp $WU/wsysconf		$SHIP/wisp/bin
cp $WU/wsysinit		$SHIP/wisp/bin
cp $WU/wusage		$SHIP/wisp/bin
cp $WU/wlicense		$SHIP/wisp/bin

echo Loading $SHIP/wisp/lib
cp $VL/libvideo.a	$SHIP/wisp/lib
cp $WL/libwisp.a	$SHIP/wisp/lib

echo Loading $SHIP/wisp/etc
cp $ETC/RELNOTES	$SHIP/wisp/etc
cp $ETC/PRINTQ.DOC	$SHIP/wisp/etc
cp $PQ/classdef.sam	$SHIP/wisp/etc
cp $WU/disprint.wcb	$SHIP/wisp/etc
cp $WU/disprntacu.umf	$SHIP/wisp/etc
cp $WU/disprntmf.umf	$SHIP/wisp/etc
cp $PQ/formdef.sam	$SHIP/wisp/etc
cp $PQ/prtdef.sam	$SHIP/wisp/etc
cp $PQ/ilpdef.sam	$SHIP/wisp/etc
cp $PORT/which.sh	$SHIP/wisp/etc/which
cp $WT/words.def	$SHIP/wisp/etc
cp $PORT/make.include	$SHIP/wisp/etc
cp $PQ/ilpremote.sh	$SHIP/wisp/etc/ilpremote.sam
cp $PQ/ilpsrv.sh	$SHIP/wisp/etc/ilpsrv.sam

cd $ETC
$WU/makemsg

echo Loading $SHIP/wisp/config
cp $ETC/forms		$SHIP/wisp/config/FORMS
cp $ETC/lgmap		$SHIP/wisp/config/LGMAP
cp $ETC/lpmap		$SHIP/wisp/config/LPMAP
cp $ETC/options.dat	$SHIP/wisp/config/OPTIONS
cp $ETC/prmap		$SHIP/wisp/config/PRMAP
cp $ETC/scmap		$SHIP/wisp/config/SCMAP
cp $ETC/wispmsg.dat	$SHIP/wisp/config
cp $ETC/wispmsg.txt	$SHIP/wisp/config
cp $ETC/wrunconfig	$SHIP/wisp/config

echo Loading $SHIP/wisp/config/videocap
for org in $VC/*.vcap
do
	new=`basename $org .vcap`
	cp $org $SHIP/wisp/config/videocap/$new
done

echo Loading $SHIP/wisp/acu
cp $WACU/acu.rules	$SHIP/wisp/acu
cp $WACU/aculink.wcb	$SHIP/wisp/acu
cp $WACU/acuusing.cob	$SHIP/wisp/acu
cp $WACU/sub85.c	$SHIP/wisp/acu
cp $WACU/wruncbl.umf	$SHIP/wisp/acu
cp $WACU/xterm.acu	$SHIP/wisp/acu

echo Loading $SHIP/wisp/mf
cp $WMF/mf.rules	$SHIP/wisp/mf
cp $WMF/mflink.cob	$SHIP/wisp/mf
cp $WMF/wispmf.c	$SHIP/wisp/mf
cp $WMF/wispmf.o	$SHIP/wisp/mf
cp $WMF/wrunmf.c	$SHIP/wisp/mf
cp $WMF/wrunmf.o	$SHIP/wisp/mf
cp $WMF/wrunmf.umf	$SHIP/wisp/mf

echo Loading $SHIP/ede
cp $EDE/good		$SHIP/ede
cp $EDE/libede.a	$SHIP/ede
cp $EDE/helpmap.unix	$SHIP/ede/demo/HELPMAP
cp $EDE/*.wcb		$SHIP/ede/demo
cp $EDE/*.hlp		$SHIP/ede/demo
cp $EDE/menudemo.opt	$SHIP/ede/demo
cp $EDE/menudemo.umf	$SHIP/ede/demo
cp $EDE/menudemomf.umf	$SHIP/ede/demo

echo Loading $SHIP/rts
cp $WACU/wruncbl	$SHIP/rts
cp $WACU/wruncble	$SHIP/rts

echo Loading $SHIP/ivs
cp $WISP/src/ivslib/libivs.a	$SHIP/ivs

echo
echo The SHIP KIT has been built.
echo If there were any errors reported then they must
echo be investigated before continuing.
echo
