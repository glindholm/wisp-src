#!/bin/sh
#
#	Copyright (c) 1988-2002 Neomedia Technologies, All rights reserved.
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
#	Input:		${WISPSRC}/...	The WISP source directory.
#
#	Output:		${WISPSRC}/wisp_XXXX.ship/...
#					The temporary ship kit.
#
#

if [ ! -d ${WISPSRC} ]
then
	echo
	echo Directory \${WISPSRC} does not exist!
	echo bldshipkit.sh ABORTING!
	exit
fi

echo
echo "Enter the WISP version number (e.g. 4400) ?"
read ANS
if [ "${ANS}" = "" ]
then
	echo Version can not be blank.
	exit 1
fi
VER=${ANS}
echo
echo Using Version=[${VER}]

#
#	Define some variables
#
WISP_VER=wisp_${VER}
EDE_VER=ede_${VER}
SHIP=${WISPSRC}/${WISP_VER}.ship
SHIPWISP=${SHIP}/${WISP_VER}
SHIPEDE=${SHIP}/${EDE_VER}
WU=${WISPSRC}/wisputils
PT=${WISPSRC}/proctran
VT=${WISPSRC}/videotest
VC=${WISPSRC}/videocap
WT=${WISPSRC}/wisptran
WL=${WISPSRC}/wisplib
VL=${WISPSRC}/videolib
DOC=${WISPSRC}/doc
ETC=${WISPSRC}/etc
PORT=${WISPSRC}/port
WACU=${WISPSRC}/acu
WMF=${WISPSRC}/mf
EDE=${WISPSRC}/ede
WPROC=${WISPSRC}/wproc

#
#	Create all the SHIP KIT directories
#
if [ -d ${SHIP} ]
then
	echo
	echo Removing OLD ${SHIP}
	rm -f -r ${SHIP}

	if [ -d ${SHIP} ]
	then
		echo Unable to remove ${SHIP}
		echo bldshipkit.sh Aborting!
		exit
	fi
fi

echo Creating ${WISPSRC}/ship
mkdir ${SHIP}
mkdir ${SHIPWISP}
mkdir ${SHIPWISP}/bin
mkdir ${SHIPWISP}/lib
mkdir ${SHIPWISP}/etc
mkdir ${SHIPWISP}/acu
mkdir ${SHIPWISP}/mf
mkdir ${SHIPWISP}/config
mkdir ${SHIPWISP}/config/videocap
mkdir ${SHIPWISP}/demo
mkdir ${SHIPEDE}
mkdir ${SHIPEDE}/demo
#mkdir ${SHIP}/ivs

#
#	Copy all the files into the ship kit
#

echo Loading ${SHIPWISP}/bin
cp ${WU}/bldmf			${SHIPWISP}/bin
cp ${WU}/display		${SHIPWISP}/bin
cp ${WU}/makemsg		${SHIPWISP}/bin
cp ${PT}/proctran		${SHIPWISP}/bin
cp ${WU}/selectpg		${SHIPWISP}/bin
cp ${WU}/vcapkeys		${SHIPWISP}/bin
cp ${WU}/viewkey		${SHIPWISP}/bin
cp ${WISPSRC}/vsedit/vsedit 	${SHIPWISP}/bin
cp ${WISPSRC}/vsedit/wac.sh 	${SHIPWISP}/bin
cp ${WU}/vsx			${SHIPWISP}/bin
cp ${VT}/vtest			${SHIPWISP}/bin
cp ${WU}/wcopy			${SHIPWISP}/bin
cp ${WU}/wdelay			${SHIPWISP}/bin
cp ${WU}/wdelwrk		${SHIPWISP}/bin
cp ${WU}/wdiag			${SHIPWISP}/bin
cp ${WU}/wexists		${SHIPWISP}/bin
cp ${WU}/wfind			${SHIPWISP}/bin
cp ${WT}/wisp			${SHIPWISP}/bin
cp ${WPROC}/wproc		${SHIPWISP}/bin
cp ${WU}/wputparm		${SHIPWISP}/bin
cp ${WU}/wrename		${SHIPWISP}/bin
cp ${WU}/wretcode		${SHIPWISP}/bin
cp ${WU}/wrun			${SHIPWISP}/bin
cp ${WU}/wscratch		${SHIPWISP}/bin
cp ${WU}/wshell			${SHIPWISP}/bin
cp ${WU}/wsort			${SHIPWISP}/bin
cp ${WU}/wsubmit		${SHIPWISP}/bin
cp ${WU}/wsysconf		${SHIPWISP}/bin
cp ${WU}/wsysinit		${SHIPWISP}/bin
cp ${WU}/wusage			${SHIPWISP}/bin
cp ${WU}/wlicense		${SHIPWISP}/bin
cp ${WU}/hexed			${SHIPWISP}/bin

echo Loading ${SHIPWISP}/lib
cp ${VL}/libvideo.a	${SHIPWISP}/lib
cp ${WL}/libwisp.a	${SHIPWISP}/lib

echo Loading ${SHIPWISP}/etc
cp ${DOC}/wisp_relnotes.txt	${SHIPWISP}/etc
cp ${WU}/DISPRINT.wcb		${SHIPWISP}/etc
cp ${WU}/disprint.umf		${SHIPWISP}/etc
cp ${WT}/words.def		${SHIPWISP}/etc
cp ${PORT}/make.include		${SHIPWISP}/etc
cp ${WU}/WISPPLAT.wcb		${SHIPWISP}/etc
cp ${WU}/SOFTLINK.wcb		${SHIPWISP}/etc
cp ${DOC}/wproc.txt		${SHIPWISP}/etc
cp ${DOC}/nonascii.txt		${SHIPWISP}/etc
cp ${DOC}/wisp_install_unix.txt	${SHIPWISP}/etc

cd ${ETC}
${WU}/makemsg

echo Loading ${SHIPWISP}/config
cp ${ETC}/FORMS		${SHIPWISP}/config
cp ${ETC}/LGMAP		${SHIPWISP}/config
cp ${ETC}/LPMAP		${SHIPWISP}/config
cp ${ETC}/OPTIONS	${SHIPWISP}/config
cp ${ETC}/PRMAP		${SHIPWISP}/config
cp ${ETC}/SCMAP		${SHIPWISP}/config
cp ${ETC}/CHARMAP	${SHIPWISP}/config
cp ${ETC}/wispmsg.dat	${SHIPWISP}/config
cp ${ETC}/wispmsg.txt	${SHIPWISP}/config
cp ${ETC}/wrunconfig	${SHIPWISP}/config
cp ${ETC}/W4WMAP	${SHIPWISP}/config
cp ${WPROC}/wproc.msg	${SHIPWISP}/config

echo Loading ${SHIPWISP}/config/videocap
cp ${VC}/*.vcap 	${SHIPWISP}/config/videocap

echo Loading ${SHIPWISP}/acu
cp ${WACU}/acu.rules		${SHIPWISP}/acu
cp ${WACU}/ACULINK.wcb		${SHIPWISP}/acu
cp ${WACU}/ACUUSING.cob		${SHIPWISP}/acu
cp ${WACU}/sub85.c		${SHIPWISP}/acu
cp ${WACU}/sub85_acu51.c	${SHIPWISP}/acu
cp ${WACU}/wruncbl.umf		${SHIPWISP}/acu
cp ${WACU}/xterm.acu		${SHIPWISP}/acu

cp ${WACU}/wacuerror.cob	${SHIPWISP}/acu
cp ${WACU}/wacudisplay.cob	${SHIPWISP}/acu
cp ${WACU}/wacufac2screen.cob	${SHIPWISP}/acu
cp ${WACU}/wacugetparm.cob	${SHIPWISP}/acu
cp ${WACU}/wacugetpfkey.cob	${SHIPWISP}/acu
cp ${WACU}/wacuhelp.cob		${SHIPWISP}/acu
cp ${WACU}/wacuwsb.cob		${SHIPWISP}/acu
cp ${WACU}/acucobol.include 	${SHIPWISP}/acu
cp ${DOC}/wispacn.txt		${SHIPWISP}/acu

echo Loading ${SHIPWISP}/mf
cp ${WMF}/mf.rules	${SHIPWISP}/mf
cp ${WMF}/MFLINK.cob	${SHIPWISP}/mf
cp ${WMF}/wispmf.c	${SHIPWISP}/mf
cp ${WMF}/wispmf.o	${SHIPWISP}/mf
cp ${WMF}/wrunmf.c	${SHIPWISP}/mf
cp ${WMF}/wrunmf.o	${SHIPWISP}/mf
cp ${WMF}/wrunmf.umf	${SHIPWISP}/mf

echo Loading ${SHIPWISP}/demo
WPROC_DEMOSTUFF="cursor.wps demo.wps dr.wps ed.wps environ.wps putparm.wps screen.wps test.wps video.wps"
for f in ${WPROC_DEMOSTUFF}
do
	cp ${WPROC}/$f	${SHIPWISP}/demo
done

echo Loading ${SHIPEDE}
cp ${EDE}/good			${SHIPEDE}
cp ${EDE}/libede.a		${SHIPEDE}
cp ${EDE}/demo/helpmap.unix	${SHIPEDE}/demo/HELPMAP
cp ${EDE}/demo/*.wcb		${SHIPEDE}/demo
cp ${EDE}/demo/*.hlp		${SHIPEDE}/demo
cp ${EDE}/demo/menudemo.opt	${SHIPEDE}/demo
cp ${EDE}/demo/menudemo.umf	${SHIPEDE}/demo
cp ${EDE}/demo/menudemomf.umf	${SHIPEDE}/demo

#echo Loading ${SHIP}/ivs
#cp ${WISPSRC}/ivslib/libivs.a	${SHIP}/ivs

INFO=${SHIPWISP}/Info
echo Building Info file ${INFO}
date						>${INFO}
uname -s -r -v					>>${INFO}
${SHIPWISP}/bin/wisp|grep Version=		>>${INFO}
${SHIPWISP}/bin/wproc -v|grep '[Vv]ersion'	>>${INFO}

cp ${DOC}/wisp_relnotes.txt		${SHIP}
cp ${DOC}/wisp_install_unix.txt		${SHIP}

echo 
echo Setting all the file modes
echo

cd ${SHIP}
find ${SHIP} -type f -print | xargs chmod a=r
find ${SHIP} -type d -print | xargs chmod u=rwx,go=rx

chmod a+x ${SHIPWISP}/bin/*
chmod a+w ${SHIPWISP}/acu/acucobol.include
chmod a+w ${SHIPWISP}/*/*.umf
chmod a+w ${SHIPWISP}/*/*.rules
chmod a+w ${SHIPWISP}/config
chmod a+w ${SHIPWISP}/config/*
chmod a+w ${SHIPWISP}/mf/*.o

chmod a+x ${SHIPEDE}/good
chmod a+w ${SHIPEDE}/demo/*.umf


cd ${SHIP}
echo Building ${WISP_VER}.tar.Z
tar -cf - ${WISP_VER} | compress > ${WISP_VER}.tar.Z

echo Building ${EDE_VER}.tar.Z
tar -cf - ${EDE_VER}  | compress > ${EDE_VER}.tar.Z

chmod a=r *.tar.Z


echo
echo The SHIP KIT has been built.
echo If there were any errors reported then they must
echo be investigated before continuing.
echo

#	History:
#	$Log: bldshipkit.sh,v $
#	Revision 1.31.2.2  2002/11/07 22:53:44  gsl
#	change to use WISPSRC variable
#	
#	Revision 1.31.2.1  2002/10/08 18:06:54  gsl
#	Updated from HEAD
#	
#	Revision 1.34  2002/06/05 14:37:23  gsl
#	removed which script
#	
#	Revision 1.33  2002/06/04 21:00:49  gsl
#	Uppercase cobol files
#	
#	Revision 1.32  2002/06/04 14:10:26  gsl
#	Changes to structure from switch to CVS from RSC
#	Mostly ETC -> DOC and EDE/demo
#	
#	Revision 1.31  2002/04/10 20:10:33  gsl
#	Change to wisp_XXXX.tar
#	
#	Revision 1.30  2002-04-10 12:46:24-04  gsl
#	Big rework to fix permissions plus setup for CDROM kits
#
#	Revision 1.29  2002-04-01 11:58:07-05  gsl
#	Add sub85_acu51.c
#
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
#			07/08/92	Add the loading of ${SHIP}/rts. GSL
#			03/11/93	Add VSEDIT. GSL
#			07/23/93	Removed CRID and added IVS. GSL
#			04/06/94	Added wac.sh for VSEDIT. GSL
#			04/13/94	Add ${SHIP}/v for video headers. GSL
