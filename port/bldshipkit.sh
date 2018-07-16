#!/bin/sh
#
#	Copyright (c) 1988-1995 DevTech Migrations, All rights reserved.
#	$Id:$
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

echo
echo "Enter the WISP version number (e.g. 4400) ?"
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
WU=$WISP/src/wisputils
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
WPROC=$WISP/src/wproc

#
#	Create all the SHIP KIT directories
#
if [ -d $SHIP ]
then
	echo
	echo Removing OLD $SHIP
	rm -f -r $SHIP

	if [ -d $SHIP ]
	then
		echo Unable to remove $SHIP
		echo bldshipkit.sh Aborting!
		exit
	fi
fi

echo Creating $WISP/src/ship
mkdir -m 755 $SHIP
mkdir -m 755 $SHIPWISP
mkdir -m 755 $SHIPWISP/bin
mkdir -m 755 $SHIPWISP/lib
mkdir -m 755 $SHIPWISP/etc
mkdir -m 755 $SHIPWISP/acu
mkdir -m 755 $SHIPWISP/mf
mkdir -m 755 $SHIPWISP/config
mkdir -m 755 $SHIPWISP/config/videocap
mkdir -m 755 $SHIPWISP/demo
mkdir -m 755 $SHIPEDE
mkdir -m 755 $SHIPEDE/demo
mkdir -m 755 $SHIP/rts
mkdir -m 755 $SHIP/ivs
mkdir -m 755 $SHIP/v

#
#	Copy all the files into the ship kit
#

echo Loading $SHIPWISP/bin
cp $WU/bldmf		$SHIPWISP/bin
cp $WU/display		$SHIPWISP/bin
cp $WU/makemsg		$SHIPWISP/bin
cp $PT/proctran		$SHIPWISP/bin
cp $WU/selectpg		$SHIPWISP/bin
cp $WU/vcapkeys		$SHIPWISP/bin
cp $WU/viewkey		$SHIPWISP/bin
cp $WISP/src/vsedit/vsedit $SHIPWISP/bin
cp $WISP/src/vsedit/wac.sh $SHIPWISP/bin
cp $WU/vsx		$SHIPWISP/bin
cp $VT/vtest		$SHIPWISP/bin
cp $WU/wcopy		$SHIPWISP/bin
cp $WU/wdelay		$SHIPWISP/bin
cp $WU/wdelwrk		$SHIPWISP/bin
cp $WU/wdiag		$SHIPWISP/bin
cp $WU/wexists		$SHIPWISP/bin
cp $WU/wfind		$SHIPWISP/bin
cp $WT/wisp		$SHIPWISP/bin
cp $WPROC/wproc		$SHIPWISP/bin
cp $WU/wputparm		$SHIPWISP/bin
cp $WU/wrename		$SHIPWISP/bin
cp $WU/wretcode		$SHIPWISP/bin
cp $WU/wrun		$SHIPWISP/bin
cp $WU/wscratch		$SHIPWISP/bin
cp $WU/wshell		$SHIPWISP/bin
cp $WU/wsort		$SHIPWISP/bin
cp $WU/wsubmit		$SHIPWISP/bin
cp $WU/wsysconf		$SHIPWISP/bin
cp $WU/wsysinit		$SHIPWISP/bin
cp $WU/wusage		$SHIPWISP/bin
cp $WU/wlicense		$SHIPWISP/bin
cp $WU/hexed		$SHIPWISP/bin
chmod 555 $SHIPWISP/bin/*

echo Loading $SHIPWISP/lib
cp $VL/libvideo.a	$SHIPWISP/lib
cp $WL/libwisp.a	$SHIPWISP/lib
chmod 444 $SHIPWISP/lib/*

echo Loading $SHIPWISP/etc
cp $ETC/RELNOTES	$SHIPWISP/etc
cp $WU/disprint.wcb	$SHIPWISP/etc
cp $WU/disprint.umf	$SHIPWISP/etc
cp $PORT/which.sh	$SHIPWISP/etc/which
cp $WT/words.def	$SHIPWISP/etc
cp $PORT/make.include	$SHIPWISP/etc
cp $WU/wispplat.wcb	$SHIPWISP/etc
cp $WU/softlink.wcb	$SHIPWISP/etc
cp $WPROC/wproc.txt	$SHIPWISP/etc
cp $ETC/nonascii.txt	$SHIPWISP/etc
cp $ETC/wisp_install_unix.txt	$SHIPWISP/etc

cd $ETC
$WU/makemsg
chmod 444 $SHIPWISP/etc/*
chmod 555 $SHIPWISP/etc/which

echo Loading $SHIPWISP/config
cp $ETC/FORMS		$SHIPWISP/config
cp $ETC/LGMAP		$SHIPWISP/config
cp $ETC/LPMAP		$SHIPWISP/config
cp $ETC/OPTIONS		$SHIPWISP/config
cp $ETC/PRMAP		$SHIPWISP/config
cp $ETC/SCMAP		$SHIPWISP/config
cp $ETC/CHARMAP		$SHIPWISP/config
cp $ETC/wispmsg.dat	$SHIPWISP/config
cp $ETC/wispmsg.txt	$SHIPWISP/config
cp $ETC/wrunconfig	$SHIPWISP/config
cp $ETC/W4WMAP		$SHIPWISP/config
cp $WPROC/wproc.msg	$SHIPWISP/config

echo Loading $SHIPWISP/config/videocap
cp $VC/*.vcap 		$SHIPWISP/config/videocap
# Used to strip off the .vcap extensions
#for org in $VC/*.vcap
#do
#	new=`basename $org .vcap`
#	cp $org $SHIPWISP/config/videocap/$new
#done
chmod 444 $SHIPWISP/config/videocap/*
chmod 444 $SHIPWISP/config/*
chmod 755 $SHIPWISP/config/videocap

echo Loading $SHIPWISP/acu
cp $WACU/acu.rules	$SHIPWISP/acu
cp $WACU/aculink.wcb	$SHIPWISP/acu
cp $WACU/acuusing.cob	$SHIPWISP/acu
cp $WACU/sub85.c	$SHIPWISP/acu
cp $WACU/wruncbl.umf	$SHIPWISP/acu
cp $WACU/xterm.acu	$SHIPWISP/acu

cp $WACU/wacuerror.cob		$SHIPWISP/acu
cp $WACU/wacudisplay.cob	$SHIPWISP/acu
cp $WACU/wacufac2screen.cob	$SHIPWISP/acu
cp $WACU/wacugetparm.cob	$SHIPWISP/acu
cp $WACU/wacugetpfkey.cob	$SHIPWISP/acu
cp $WACU/wacuhelp.cob		$SHIPWISP/acu
cp $WACU/wacuwsb.cob		$SHIPWISP/acu
cp $ETC/wispacn.txt		$SHIPWISP/acu

chmod 444 $SHIPWISP/acu/*

cp $WACU/acucobol.include $SHIPWISP/acu
chmod 666 $SHIPWISP/acu/acucobol.include

echo Loading $SHIPWISP/mf
cp $WMF/mf.rules	$SHIPWISP/mf
cp $WMF/mflink.cob	$SHIPWISP/mf
cp $WMF/wispmf.c	$SHIPWISP/mf
cp $WMF/wispmf.o	$SHIPWISP/mf
cp $WMF/wrunmf.c	$SHIPWISP/mf
cp $WMF/wrunmf.o	$SHIPWISP/mf
cp $WMF/wrunmf.umf	$SHIPWISP/mf
chmod 444 $SHIPWISP/mf/*

echo Loading $SHIPWISP/demo
WPROC_DEMOSTUFF="cursor.wps demo.wps dr.wps ed.wps environ.wps putparm.wps screen.wps test.wps video.wps"
for f in $WPROC_DEMOSTUFF
do
	cp $WPROC/$f	$SHIPWISP/demo
done

echo Loading $SHIPEDE
cp $EDE/good		$SHIPEDE
cp $EDE/libede.a	$SHIPEDE
cp $EDE/helpmap.unix	$SHIPEDE/demo/HELPMAP
cp $EDE/*.wcb		$SHIPEDE/demo
cp $EDE/*.hlp		$SHIPEDE/demo
cp $EDE/menudemo.opt	$SHIPEDE/demo
cp $EDE/menudemo.umf	$SHIPEDE/demo
cp $EDE/menudemomf.umf	$SHIPEDE/demo
chmod 444 $SHIPEDE/demo/*
chmod 444 $SHIPEDE/*
chmod 755 $SHIPEDE/demo
chmod 555 $SHIPEDE/good

echo Loading $SHIP/rts
cp $WACU/wruncbl	$SHIP/rts
cp $WACU/wruncble	$SHIP/rts
chmod 555 $SHIP/rts/*

echo Loading $SHIP/ivs
cp $WISP/src/ivslib/libivs.a	$SHIP/ivs
chmod 444 $SHIP/ivs/*

echo Loading $SHIP/v
cp $WISP/src/videolib/*.h $SHIP/v
chmod 444 $SHIP/v/*

INFO=$SHIPWISP/Info
echo Building Info file $INFO
date					>$INFO
uname -s -r -v				>>$INFO
$SHIPWISP/bin/wisp|grep Version=	>>$INFO
$SHIPWISP/bin/wproc -v|grep '[Vv]ersion'>>$INFO
chmod 444 $INFO

if [ -f $SHIP/rts/wruncbl ]
then
	INFO=$SHIP/rts/Info
	echo Building Info file $INFO
	date					>$INFO
	uname -s -r -v				>>$INFO
	$SHIPWISP/bin/wisp|grep Version=	>>$INFO
	$SHIP/rts/wruncbl -V			>>$INFO 2>&1
	chmod 444 $INFO
fi

cd $SHIP
echo Building wisp$VER.tar.Z
tar -cf - wisp.$VER | compress > wisp$VER.tar.Z

echo Building ede$VER.tar.Z
tar -cf - ede.$VER | compress > ede$VER.tar.Z

echo
echo The SHIP KIT has been built.
echo If there were any errors reported then they must
echo be investigated before continuing.
echo

#	History:
#	$Log: bldshipkit.sh,v $
#	Revision 1.28  2001-10-16 11:30:25-04  gsl
#	Add hexed
#
#	Revision 1.27  2001-10-11 13:45:10-04  gsl
#	Leave the .vcap extensions on the videocap files
#
#	Revision 1.26  2001-10-09 16:21:14-04  gsl
#	wproc.lis -> wproc.txt
#	Add wisp_install_unix.txt
#
#	Revision 1.25  1998-04-03 13:53:46-05  gsl
#	Add wacudisplay.cob
#
#	Revision 1.24  1997-12-19 15:08:19-05  gsl
#	Renamed much of the wisp/etc routines
#
#	Revision 1.23  1997-12-19 14:38:20-05  gsl
#	Add nonascii.txt
#
#	Revision 1.22  1997-12-15 10:22:29-05  gsl
#	Add CHARMAP
#
#	Revision 1.21  1997-10-29 11:50:37-05  gsl
#	Add wacuwsb.cob
#
#	Revision 1.20  1997-10-02 09:13:09-04  gsl
#	remove ed.wpr
#
#	Revision 1.19  1997-09-30 10:32:51-04  gsl
#	Add Acucobol Native Screens stuff
#
#	Revision 1.18  1997-06-02 11:07:01-04  gsl
#	Replaced disprntacu.umf and disprntmf.umf with disprint.umf
#
#	Revision 1.17  1997-05-20 14:11:23-04  scass
#	Added inclusion of acucobol.include to acu directory
#	of WISP Toolkit
#
#	Revision 1.16  1997-03-26 11:25:30-05  gsl
#	Add WPROC into the wisp kit
#	Build the compress kits
#
#	Revision 1.15  1996-11-08 16:51:53-05  gsl
#	Add softlink.wcb
#
#	Revision 1.14  1996-11-07 16:13:34-08  gsl
#	Add WISPPLAT.WCB
#
#	Revision 1.13  1996-07-26 10:25:19-07  gsl
#	Fix to load the video headers from videolib instead of include/v
#
#	Revision 1.12  1996-07-24 11:13:13-07  gsl
#	Use the new wshell program instead of the old wshell script
#
#	Revision 1.11  1996-01-05 08:03:40-08  gsl
#	fixed syntax error
#
# Revision 1.10  1996/01/05  16:02:48  gsl
# Added version numbers to the wisp shipping kits
#
#			06/09/92	Written by GSL
#			06/12/92	Add ilpremote/ilpsrv, wshell, make.include. GSL
#			06/30/92	Add wdiag. GSL
#			07/08/92	Add info messages. GSL
#			07/08/92	Add the loading of $SHIP/rts. GSL
#			03/11/93	Add VSEDIT. GSL
#			07/23/93	Removed CRID and added IVS. GSL
#			04/06/94	Added wac.sh for VSEDIT. GSL
#			04/13/94	Add $SHIP/v for video headers. GSL
