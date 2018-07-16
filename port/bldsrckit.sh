#!/bin/ksh
#
#	Copyright (c) 1995-1997 NeoMedia Technologies, All rights reserved.
#	$Id:$
#
#
#	File:		bldsrckit.sh
#
#	Function:	This script builds the WISP source kit
#
#	Desciption:	This routine can only be run from zigzag.  
#			It builds the WISP source kit.
#			It unloads the source for DRCS.
#
#			- build the directories
#			- unload each component
#			- copy the ".umf" files to "Makefile"
#			- sample config files have ".unix" extension removed
#			- all ".obj" and ".com" files are deleted
#			- all long files (15+ chars are removed)
#			- all ".sh" files have x bit set (chmod +x)
#
#	Warning:	If the DRCS directory structures change or new ones
#			are added then this routine must be updated.
#

echo This script will build the WISP source kit.
echo
echo It creates all the directory needed and then uses DRCS to unload all the 
echo source files from their DRCS directories on ZIGZAG.
echo
echo -n	'Do you wish to continue [y/n] ? ' 
read ANS
if [ "y" != "${ANS}" ]
then
echo	exiting
exit
else
echo	continuing...
fi

#
#	WISP_PROJ_LIST is a list of all the projects in DRCS that 
#	are used in the WISP source kit.  It is used to set the
#	RCS states and to create the Versions.lis file.
#
WISP_PROJ_LIST="
video/cap
video/ivs
video/lib
video/test
wisp/acucobol
wisp/amu
wisp/common
wisp/costar
wisp/doc
wisp/ede
wisp/etc
wisp/ivs
wisp/kcsi/create
wisp/kcsi/crid
wisp/kcsi/common
wisp/lib
wisp/menudemo
wisp/mf
wisp/msdos
wisp/nt
wisp/port
wisp/proctran
wisp/sample
wisp/tran
wisp/utils
wisp/vms
wisp/vsedit
wisp/wproc"

DISAM_PROJ_LIST="
disam.34/code
disam.34/decs
disam.34/docs
disam.34/head
disam.34/make
disam.34/port
disam.34/test
disam.34/tp.21
disam.34/util
disam.34/mfisam"

PARENTDIR=`pwd`
echo
echo -n	"Enter parent directory [$PARENTDIR] ? "
read ANS
if [ "$ANS" != "" ]
then
	cd $ANS
	PARENTDIR=`pwd`
fi

echo pwd = `pwd`
echo
echo -n	"Enter name of save dir (e.g. wisp3318) ? "
read ANS
if [ "$ANS" = "" ]
then
	ANS=srcXXX
fi

#
#	Make all the directories needed
#

echo mkdir $ANS
test -d $ANS || mkdir $ANS

echo cd $ANS
cd $ANS

echo mkdir src
test -d src || mkdir src

echo cd src
cd src

SOURCEDIR=`pwd`

WISPDIRLIST="
	acu \
	amu \
	ede \
	etc \
	ivslib \
	kcsi \
	kcsi/common \
	kcsi/create \
	kcsi/crid \
	lib \
	mf \
	msdos \
	nt \
	port \
	proctran \
	testacu \
	testmf \
	videolib \
	videotest \
	videocap \
	vsedit \
	wispcommon \
	wisplib \
	wisptran \
	wisputils \
	wproc \
	"

DISAMDIRLIST="
	kcsi/disam \
	"

echo Creating directories ...
for i in $WISPDIRLIST
do
	if [ -d $i ]
	then
		echo Dir $i already exists
	else
		echo mkdir $i
		mkdir $i
		if [ ! -d $i ]
		then
			echo ERROR mkdir $i failed
			exit 1
		fi
	fi
done

#
#	SET the RCS STATES
#

echo
echo "This script can set the RCS states of all the files in WISP to"
echo "a new version number (e.g. V3_3_18)."
echo
echo -n	'Do you wish to set the RCS states [y/n] ? ' 

read ANS
if [ "y" != "${ANS}" ]
then
	echo
	echo	RCS States will not be changed.
else
	#
	#	Get the version number to use
	#
	echo
	echo -n	"Enter the state to use (e.g. V3_3_18) ? "
	read ANS
	if [ "$ANS" = "" ]
	then
		echo state can not be blank.
		exit 1
	fi
	the_state=$ANS
	echo
	echo Using state=[$the_state]

	#
	#	Set the state for each project
	#
	for the_proj in $WISP_PROJ_LIST
	do
		echo Setting state for project $the_proj
		drcs state $the_state $the_proj
	done

	echo 
	echo 
	echo RCS states have been set to $the_state
	echo 
fi


#
#	Down load all the SCS files into the source kit.
#

#echo ' '
#echo 'Your choices for each project/component are y=yes, n=no, a=all or q=quit'
#echo ' '

ANS=a

echo
echo ==================== VIDEO LIB ===========================================
echo
echo cd $SOURCEDIR/videolib
cd $SOURCEDIR/videolib

echo pwd = `pwd`
if [ "a" != "$ANS" ]
then
	echo -n	'Load VIDEO LIB [y/n/a/q] ? '
	read ANS
fi
if [ "q" = "$ANS" ]
then
	echo 'bldsrckit.sh: aborted.'
	exit 1
fi
if [ "y" = "${ANS}" -o "a" = "${ANS}" ]
then
echo	Loading VIDEO LIB
	drcs unload video/lib

	cp libvideo.umf Makefile
fi

echo
echo =================== VIDEO TEST ===========================================
echo
echo cd $SOURCEDIR/videotest
cd $SOURCEDIR/videotest

echo pwd = `pwd`
if [ "a" != "$ANS" ]
then
	echo -n	'Load VIDEO TEST [y/n/a/q] ? '
	read ANS
fi
if [ "q" = "$ANS" ]
then
	echo 'bldsrckit.sh: aborted.'
	exit 1
fi
if [ "y" = "${ANS}" -o "a" = "${ANS}" ]
then
echo	Loading VIDEO TEST
	drcs unload video/test

	cp vtest.umf Makefile
fi

echo
echo =================== VIDEO CAP ===========================================
echo
echo cd $SOURCEDIR/videocap
cd $SOURCEDIR/videocap

echo pwd = `pwd`
if [ "a" != "$ANS" ]
then
	echo -n	'Load VIDEO CAP [y/n/a/q] ? '
	read ANS
fi
if [ "q" = "$ANS" ]
then
	echo 'bldsrckit.sh: aborted.'
	exit 1
fi
if [ "y" = "${ANS}" -o "a" = "${ANS}" ]
then
echo	Loading VIDEO CAP
	drcs unload video/cap
fi

echo
echo =================== VIDEO IVS LIB ========================================
echo
echo cd $SOURCEDIR/ivslib
cd $SOURCEDIR/ivslib

echo pwd = `pwd`
if [ "a" != "$ANS" ]
then
	echo -n	'Load VIDEO IVS LIB [y/n/a/q] ? '
	read ANS
fi
if [ "q" = "$ANS" ]
then
	echo 'bldsrckit.sh: aborted.'
	exit 1
fi
if [ "y" = "${ANS}" -o "a" = "${ANS}" ]
then
echo	Loading VIDEO IVS LIB
	drcs unload video/ivs

	cp libivs.umf Makefile
fi


echo
echo ==================== WISP COMMON =====================================
echo
echo cd $SOURCEDIR/wispcommon
cd $SOURCEDIR/wispcommon
echo Loading WISP COMMON
drcs unload wisp/common

echo
echo ================== WISP ACUCOBOL =======================================
echo
echo cd $SOURCEDIR/acu
cd $SOURCEDIR/acu

echo pwd = `pwd`
if [ "a" != "$ANS" ]
then
	echo -n	'Load WISP ACUCOBOL [y/n/a/q] ? '
	read ANS
fi
if [ "q" = "$ANS" ]
then
	echo 'bldsrckit.sh: aborted.'
	exit 1
fi
if [ "y" = "${ANS}" -o "a" = "${ANS}" ]
then
echo	Loading WISP ACUCOBOL
	drcs unload wisp/acucobol

	chmod +w wruncbl.umf
	cp wruncbl.umf Makefile
fi

echo
echo ==================== WISP MF ===========================================
echo
echo cd $SOURCEDIR/mf
cd $SOURCEDIR/mf

echo pwd = `pwd`
if [ "a" != "$ANS" ]
then
	echo -n	'Load WISP MF [y/n/a/q] ? '
	read ANS
fi
if [ "q" = "$ANS" ]
then
	echo 'bldsrckit.sh: aborted.'
	exit 1
fi
if [ "y" = "${ANS}" -o "a" = "${ANS}" ]
then
echo	Loading WISP MF
	drcs unload wisp/mf

	chmod +w wrunmf.umf
	cp wrunmf.umf Makefile
fi

echo
echo ==================== WISP NT ===========================================
echo
echo cd $SOURCEDIR/nt
cd $SOURCEDIR/nt

echo pwd = `pwd`
if [ "a" != "$ANS" ]
then
	echo -n	'Load WISP NT [y/n/a/q] ? '
	read ANS
fi
if [ "q" = "$ANS" ]
then
	echo 'bldsrckit.sh: aborted.'
	exit 1
fi
if [ "y" = "${ANS}" -o "a" = "${ANS}" ]
then
echo	Loading WISP NT
	drcs unload wisp/nt

fi

echo
echo ================== WISP PORT ===========================================
echo
echo cd $SOURCEDIR/port
cd $SOURCEDIR/port

echo pwd = `pwd`
if [ "a" != "$ANS" ]
then
	echo -n	'Load WISP PORT [y/n/a/q] ? '
	read ANS
fi
if [ "q" = "$ANS" ]
then
	echo 'bldsrckit.sh: aborted.'
	exit 1
fi
if [ "y" = "${ANS}" -o "a" = "${ANS}" ]
then
echo	Loading WISP PORT
	drcs unload wisp/port

	cp makewisp.umf Makefile
	chmod +w make.include
fi

echo
echo =================== WISP EDE ===========================================
echo
echo cd $SOURCEDIR/ede
cd $SOURCEDIR/ede

echo pwd = `pwd`
if [ "a" != "$ANS" ]
then
	echo -n	'Load WISP EDE [y/n/a/q] ? '
	read ANS
fi
if [ "q" = "$ANS" ]
then
	echo 'bldsrckit.sh: aborted.'
	exit 1
fi
if [ "y" = "${ANS}" -o "a" = "${ANS}" ]
then
echo	Loading WISP EDE
	drcs unload wisp/ede
	drcs unload wisp/menudemo

	cp libede.umf Makefile
fi

echo
echo ==================== WISP ETC ===========================================
echo
echo cd $SOURCEDIR/etc
cd $SOURCEDIR/etc

echo pwd = `pwd`
if [ "a" != "$ANS" ]
then
	echo -n	'Load WISP ETC [y/n/a/q] ? '
	read ANS
fi
if [ "q" = "$ANS" ]
then
	echo 'bldsrckit.sh: aborted.'
	exit 1
fi
if [ "y" = "${ANS}" -o "a" = "${ANS}" ]
then
echo	Loading WISP ETC
	drcs unload wisp/etc

	for i in `drcs dir wisp/doc | sed -n '/relnotes/p'`
	do
		drcs borrow $i wisp/doc
	done

	drcs borrow qawisp.lis wisp/doc
	drcs borrow qaede.doc wisp/doc
	drcs borrow portunix.lis wisp/doc
	drcs borrow portwin32.txt wisp/doc
	drcs borrow wispntdoc.txt wisp/doc
	drcs borrow wispntsetup.txt wisp/doc
	drcs borrow wispacn.txt wisp/doc
	drcs borrow aqmwisp.txt wisp/doc
	drcs borrow nonascii.txt wisp/doc
	drcs borrow vcolors.txt wisp/doc

	for i in v*_relnotes.lis
	do
		latest_relnotes=$i
	done
	echo "cp $latest_relnotes RELNOTES"
	cp $latest_relnotes RELNOTES
fi

echo
echo =================== WISP LIB ===========================================
echo
echo cd $SOURCEDIR/wisplib
cd $SOURCEDIR/wisplib

echo pwd = `pwd`
if [ "a" != "$ANS" ]
then
	echo -n	'Load WISP LIB [y/n/a/q] ? '
	read ANS
fi
if [ "q" = "$ANS" ]
then
	echo 'bldsrckit.sh: aborted.'
	exit 1
fi
if [ "y" = "${ANS}" -o "a" = "${ANS}" ]
then
echo	Loading WISP LIB
	drcs unload wisp/lib

	cp libwisp.umf Makefile
fi

echo
echo ================= WISP PROCTRAN ========================================
echo
echo cd $SOURCEDIR/proctran
cd $SOURCEDIR/proctran

echo pwd = `pwd`
if [ "a" != "$ANS" ]
then
	echo -n	'Load WISP PROCTRAN [y/n/a/q] ? '
	read ANS
fi
if [ "q" = "$ANS" ]
then
	echo 'bldsrckit.sh: aborted.'
	exit 1
fi
if [ "y" = "${ANS}" -o "a" = "${ANS}" ]
then
echo	Loading WISP PROCTRAN
	drcs unload wisp/proctran

	cp proctran.umf Makefile
fi
echo
echo ================= WISP VSEDIT ========================================
echo
echo cd $SOURCEDIR/vsedit
cd $SOURCEDIR/vsedit

echo pwd = `pwd`
if [ "a" != "$ANS" ]
then
	echo -n	'Load WISP VSEDIT [y/n/a/q] ? '
	read ANS
fi
if [ "q" = "$ANS" ]
then
	echo 'bldsrckit.sh: aborted.'
	exit 1
fi
if [ "y" = "${ANS}" -o "a" = "${ANS}" ]
then
echo	Loading WISP VSEDIT
	drcs unload wisp/vsedit

	cp vsedit.umf Makefile
fi

echo
echo =================== WISP TEST ===========================================
echo
echo cd $SOURCEDIR/testacu
cd $SOURCEDIR/testacu

echo pwd = `pwd`
if [ "a" != "$ANS" ]
then
	echo -n	'Load WISP SAMPLE [y/n/a/q] ? '
	read ANS
fi
if [ "q" = "$ANS" ]
then
	echo 'bldsrckit.sh: aborted.'
	exit 1
fi
if [ "y" = "${ANS}" -o "a" = "${ANS}" ]
then
echo	Loading WISP SAMPLE
	drcs unload wisp/sample

	echo Duplicating testacu into testmf
	cp * ../testmf

	cp sampleacu.umf Makefile
	cp ../testmf/samplemf.umf ../testmf/Makefile

	echo Copying ACU files to testacu
	cp $SOURCEDIR/acu/acu.rules    $SOURCEDIR/testacu
	cp $SOURCEDIR/acu/aculink.wcb  $SOURCEDIR/testacu
	cp $SOURCEDIR/acu/acuusing.cob $SOURCEDIR/testacu

	echo Copying MF files to testmf
	cp $SOURCEDIR/mf/mf.rules      $SOURCEDIR/testmf
	cp $SOURCEDIR/mf/mflink.cob    $SOURCEDIR/testmf
fi

echo
echo =================== WISP TRAN ===========================================
echo
echo cd $SOURCEDIR/wisptran
cd $SOURCEDIR/wisptran

echo pwd = `pwd`
if [ "a" != "$ANS" ]
then
	echo -n	'Load WISP TRAN [y/n/a/q] ? '
	read ANS
fi
if [ "q" = "$ANS" ]
then
	echo 'bldsrckit.sh: aborted.'
	exit 1
fi
if [ "y" = "${ANS}" -o "a" = "${ANS}" ]
then
echo	Loading WISP TRAN
	drcs unload wisp/tran

	cp wisp.umf Makefile
fi

echo
echo ================= WISP UTILS ===========================================
echo
echo cd $SOURCEDIR/wisputils
cd $SOURCEDIR/wisputils

echo pwd = `pwd`
if [ "a" != "$ANS" ]
then
	echo -n	'Load WISP UTILS [y/n/a/q] ? '
	read ANS
fi
if [ "q" = "$ANS" ]
then
	echo 'bldsrckit.sh: aborted.'
	exit 1
fi
if [ "y" = "${ANS}" -o "a" = "${ANS}" ]
then
echo	Loading WISP UTILS
	drcs unload wisp/utils

	cp utils.umf Makefile
fi


echo
echo ================== WISP MSDOS ===========================================
echo
echo cd $SOURCEDIR/msdos
cd $SOURCEDIR/msdos

echo pwd = `pwd`
if [ "a" != "$ANS" ]
then
	echo -n	'Load WISP MSDOS [y/n/a/q] ? '
	read ANS
fi
if [ "q" = "$ANS" ]
then
	echo 'bldsrckit.sh: aborted.'
	exit 1
fi
if [ "y" = "${ANS}" -o "a" = "${ANS}" ]
then
echo	Loading WISP MSDOS
	drcs unload wisp/msdos

	drcs borrow autoexec.wsp wisp/doc
	drcs borrow config.wsp wisp/doc
	drcs borrow doswisp.lis wisp/doc
	drcs borrow readme.dos wisp/doc
fi

echo
echo =================== WISP WPROC ===========================================
echo
echo cd $SOURCEDIR/wproc
cd $SOURCEDIR/wproc

echo pwd = `pwd`
if [ "a" != "$ANS" ]
then
	echo -n	'Load WISP WPROC [y/n/a/q] ? '
	read ANS
fi
if [ "q" = "$ANS" ]
then
	echo 'bldsrckit.sh: aborted.'
	exit 1
fi
if [ "y" = "${ANS}" -o "a" = "${ANS}" ]
then
echo	Loading WISP WPROC
	drcs unload wisp/wproc

	chmod +w Makefile
	cp wproc.umf Makefile

	drcs borrow wproc.lis wisp/doc
fi

echo
echo =================== WISP AMU ===========================================
echo
echo cd $SOURCEDIR/amu
cd $SOURCEDIR/amu

echo pwd = `pwd`
if [ "a" != "$ANS" ]
then
	echo -n	'Load WISP AMU [y/n/a/q] ? '
	read ANS
fi
if [ "q" = "$ANS" ]
then
	echo 'bldsrckit.sh: aborted.'
	exit 1
fi
if [ "y" = "${ANS}" -o "a" = "${ANS}" ]
then
echo	Loading WISP AMU
	drcs unload wisp/amu
fi

echo
echo =================== KCSI ===========================================
echo
echo cd $SOURCEDIR/kcsi
cd $SOURCEDIR/kcsi

echo pwd = `pwd`
if [ "a" != "$ANS" ]
then
	echo -n	'Load KCSI [y/n/a/q] ? '
	read ANS
fi
if [ "q" = "$ANS" ]
then
	echo 'bldsrckit.sh: aborted.'
	exit 1
fi
if [ "y" = "${ANS}" -o "a" = "${ANS}" ]
then
echo	Loading KCSI
	cd $SOURCEDIR/kcsi/common
	drcs unload wisp/kcsi/common

	cd $SOURCEDIR/kcsi/create
	drcs unload wisp/kcsi/create
	drcs unload wisp/kcsi/common
	drcs borrow createntsetup.txt wisp/kcsi/create

	cd $SOURCEDIR/kcsi/crid
	drcs unload wisp/kcsi/crid
	drcs unload wisp/kcsi/common
	drcs borrow cridntsetup.txt wisp/kcsi/crid

#	cd $SOURCEDIR/kcsi/disam
#	drcs unload disam.34/code
#	drcs unload disam.34/decs
#	drcs unload disam.34/docs
#	drcs unload disam.34/head
#	drcs unload disam.34/make
#	drcs unload disam.34/port
#	drcs unload disam.34/test
#	drcs unload disam.34/tp.21
#	drcs unload disam.34/util
#	drcs unload disam.34/mfisam
fi

echo
echo ================== Down Loading Complete ===============================
echo
echo
echo cd $SOURCEDIR
cd $SOURCEDIR

echo pwd = `pwd`
echo
echo 'Deleting all *.com and *.obj files'
echo 'find . -name "*.com" -print | xargs rm'
find . -name "*.com" -print | xargs rm
echo 'find . -name "*.obj" -print | xargs rm'
find . -name "*.obj" -print | xargs rm

#
#	File name cleanup
#
echo " "
echo "Changing mode of all .sh files to ug+rx."
find . -name '*.sh' -print | tee /dev/tty | xargs chmod ug+rx

#
#	Create a file version list
#
echo cd $SOURCEDIR
cd $SOURCEDIR

for i in $WISP_PROJ_LIST
do
	echo Versions for $i
	drcs vers $i >>Versions.lis
done

echo 
echo "The latest release notes [" $latest_relnotes "] have"
echo "been copied to etc/RELNOTES"
echo 
echo 
echo DONE.
echo 
exit
#
#	History:
#	$Log: bldsrckit.sh,v $
#	Revision 1.33  1998-01-08 11:30:08-05  gsl
#	Add vcolors.txt
#
#	Revision 1.32  1997-12-19 17:22:14-05  gsl
#	Add aqmwisp.txt to wisp/etc
#
#	Revision 1.31  1997-12-19 15:09:36-05  gsl
#	removed the wisp/etc *.unix stuff
#
#	Revision 1.30  1997-12-19 14:35:42-05  gsl
#	Add nonascii.txt
#
#	Revision 1.29  1997-12-02 09:48:11-05  gsl
#	Remove the NT code for WISPTRAN and WCONFIG
#
#	Revision 1.28  1997-09-30 10:28:38-04  gsl
#	add wispacn.txt
#
#	Revision 1.27  1997-08-28 16:15:30-04  gsl
#	Removed DISAM from the source kit
#
#	Revision 1.26  1997-05-02 23:10:42-04  gsl
#	move drcs states stuff after all other questions
#
#	Revision 1.25  1997-04-18 15:22:19-04  gsl
#	Add the NT setup.txt files
#
#	Revision 1.24  1997-03-27 11:38:44-05  gsl
#	Add docs to etc
#
#	Revision 1.23  1997-03-25 17:16:49-05  gsl
#	Fix the kcsi common stuff
#
#	Revision 1.22  1997-03-25 15:20:25-05  gsl
#	Add NT plus remove wisp/common from all the projects
#
#	Revision 1.21  1996-09-12 12:01:26-04  gsl
#	Add WISP/NT source directory
#
#	Revision 1.20  1996-07-26 10:22:58-07  gsl
#	Remove the include/v directory
#
#	Revision 1.19  1996-07-26 10:02:49-07  gsl
#	Ensure that wisp/common was always unloaded first in each project
#
#	Revision 1.18  1996-04-10 06:38:30-07  gsl
#	New disam 3.4 stuff for KCSI utilities
#
# 	Revision 1.17  1996/01/11  15:02:03  gsl
# 	Add wisp/kcsi/common
# 	Fix mode for .sh files
#
# 	Revision 1.16  1996/01/05  13:31:50  gsl
# 	*** empty log message ***
#
#	06/04/92	Written. GSL
#	06/09/92	Remove src/bin directory. GSL
#	06/09/92	Added copy of .umf to Makefile. GSL
#	06/30/92	Split src/test into src/testacu & src/testmf. GSL
#	07/07/92	Added wisp common include to printq. GSL
#       12/14/92        Added libivs and vse JEC
#	04/29/93	Completed WISP SAMPLE section. GSL
#       04/29/93        Slight change to some Unix commands for better efficiency. JEC
#	04/11/94	Added wproc. GSL
#	05/13/94	Added AMU. GSL
#	06/08/94	MS-DOS Docs and V*_RELNOTES. GSL
#	07/07/94	ILP -> UNIQUE . GSL
#	02/01/95	Change to use DRCS
