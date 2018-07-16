#!/bin/ksh
#
#	Copyright (c) 1995-2001 NeoMedia Technologies, All rights reserved.
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

#ECHONONL="echo -n"
ECHONONL=echo

echo This script will build the WISP source kit.
echo
echo It creates all the directory needed and then uses DRCS to unload all the 
echo source files from their DRCS directories on ZIGZAG.
echo
$ECHONONL	'Do you wish to continue [y/n] ? ' 
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
video
video/cap
video/ivs
video/lib
video/test
wisp
wisp/acucobol
wisp/amu
wisp/common
wisp/costar
wisp/doc
wisp/ede
wisp/etc
wisp/ivs
wisp/kcsi
wisp/kcsi/create
wisp/kcsi/crid
wisp/kcsi/common
wisp/lib
wisp/menudemo
wisp/mf
wisp/nt
wisp/port
wisp/proctran
wisp/sample
wisp/tran
wisp/utils
wisp/vms
wisp/vsedit
wisp/wproc"

PARENTDIR=$HOME/work
echo
$ECHONONL	"Enter parent directory [$PARENTDIR] ? "
read ANS
if [ "$ANS" != "" ]
then
	cd $ANS
	PARENTDIR=$ANS
fi
cd $PARENTDIR

echo pwd = `pwd`
echo
$ECHONONL	"Enter name of save dir (e.g. wisp4400) ? "
read ANS
if [ "$ANS" = "" ]
then
	ANS=srcXXX
fi

WISPXXXX=$ANS

#
#	Make all the directories needed
#
MKDIR="mkdir -p"

echo $MKDIR $WISPXXXX
test -d $WISPXXXX || $MKDIR $WISPXXXX

echo cd $WISPXXXX
cd $WISPXXXX

echo $MKDIR src
test -d src || $MKDIR src

echo cd src
cd src

SOURCEDIR=`pwd`

WISPDIRLIST="
	acu \
	amu \
	costar \
	ede \
	etc \
	ivslib \
	kcsi \
	kcsi/create \
	kcsi/crid \
	lib \
	mf \
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

echo Creating directories ...
for i in $WISPDIRLIST
do
	if [ -d $i ]
	then
		echo Dir $i already exists
	else
		echo $MKDIR $i
		$MKDIR $i
		if [ ! -d $i ]
		then
			echo ERROR $MKDIR $i failed
			exit 1
		fi
	fi
done

#
#	SET the RCS STATES
#

echo
echo "This script can set the RCS states of all the files in WISP to"
echo "a new version number (e.g. V4_4_00)."
echo
$ECHONONL	'Do you wish to set the RCS states [y/n] ? ' 

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
	$ECHONONL	"Enter the state to use (e.g. V3_3_18) ? "
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
#	Down load all the RCS files into the source kit.
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
	$ECHONONL	'Load VIDEO LIB [y/n/a/q] ? '
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
fi

echo
echo =================== VIDEO TEST ===========================================
echo
echo cd $SOURCEDIR/videotest
cd $SOURCEDIR/videotest

echo pwd = `pwd`
if [ "a" != "$ANS" ]
then
	$ECHONONL	'Load VIDEO TEST [y/n/a/q] ? '
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
fi

echo
echo =================== VIDEO CAP ===========================================
echo
echo cd $SOURCEDIR/videocap
cd $SOURCEDIR/videocap

echo pwd = `pwd`
if [ "a" != "$ANS" ]
then
	$ECHONONL	'Load VIDEO CAP [y/n/a/q] ? '
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
	$ECHONONL	'Load VIDEO IVS LIB [y/n/a/q] ? '
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
	$ECHONONL	'Load WISP ACUCOBOL [y/n/a/q] ? '
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
	drcs borrow wispicon.ico wisp/common

	chmod +w *.umf *.mak acucobol.include acu.rules
fi

echo
echo ==================== WISP MF ===========================================
echo
echo cd $SOURCEDIR/mf
cd $SOURCEDIR/mf

echo pwd = `pwd`
if [ "a" != "$ANS" ]
then
	$ECHONONL	'Load WISP MF [y/n/a/q] ? '
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
fi

echo
echo ==================== WISP NT ===========================================
echo
echo cd $SOURCEDIR/nt
cd $SOURCEDIR/nt

echo pwd = `pwd`
if [ "a" != "$ANS" ]
then
	$ECHONONL	'Load WISP NT [y/n/a/q] ? '
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
	$ECHONONL	'Load WISP PORT [y/n/a/q] ? '
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

	cp wisp.dsw ..
	chmod +w ../wisp.dsw
fi

echo
echo =================== WISP EDE ===========================================
echo
echo cd $SOURCEDIR/ede
cd $SOURCEDIR/ede

echo pwd = `pwd`
if [ "a" != "$ANS" ]
then
	$ECHONONL	'Load WISP EDE [y/n/a/q] ? '
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
fi

echo
echo ==================== WISP ETC ===========================================
echo
echo cd $SOURCEDIR/etc
cd $SOURCEDIR/etc

echo pwd = `pwd`
if [ "a" != "$ANS" ]
then
	$ECHONONL	'Load WISP ETC [y/n/a/q] ? '
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
	drcs borrow nttelnet.txt wisp/doc
	drcs borrow wisp_install_unix.txt wisp/doc

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
	$ECHONONL	'Load WISP LIB [y/n/a/q] ? '
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
fi

echo
echo ================= WISP PROCTRAN ========================================
echo
echo cd $SOURCEDIR/proctran
cd $SOURCEDIR/proctran

echo pwd = `pwd`
if [ "a" != "$ANS" ]
then
	$ECHONONL	'Load WISP PROCTRAN [y/n/a/q] ? '
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
fi
echo
echo ================= WISP VSEDIT ========================================
echo
echo cd $SOURCEDIR/vsedit
cd $SOURCEDIR/vsedit

echo pwd = `pwd`
if [ "a" != "$ANS" ]
then
	$ECHONONL	'Load WISP VSEDIT [y/n/a/q] ? '
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
fi

echo
echo =================== WISP TEST ===========================================
echo
echo cd $SOURCEDIR/testacu
cd $SOURCEDIR/testacu

echo pwd = `pwd`
if [ "a" != "$ANS" ]
then
	$ECHONONL	'Load WISP SAMPLE [y/n/a/q] ? '
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

	echo Copying ACU files to testacu
	cp $SOURCEDIR/acu/acu.rules    $SOURCEDIR/testacu
	cp $SOURCEDIR/acu/aculink.wcb  $SOURCEDIR/testacu
	cp $SOURCEDIR/acu/acuusing.cob $SOURCEDIR/testacu

	echo Copying MF files to testmf
	cp $SOURCEDIR/mf/mf.rules      $SOURCEDIR/testmf
	cp $SOURCEDIR/mf/mflink.cob    $SOURCEDIR/testmf

	echo Changing modes
	chmod +w lgmap.nt sampleacu.mak
fi

echo
echo =================== WISP TRAN ===========================================
echo
echo cd $SOURCEDIR/wisptran
cd $SOURCEDIR/wisptran

echo pwd = `pwd`
if [ "a" != "$ANS" ]
then
	$ECHONONL	'Load WISP TRAN [y/n/a/q] ? '
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
fi

echo
echo ================= WISP UTILS ===========================================
echo
echo cd $SOURCEDIR/wisputils
cd $SOURCEDIR/wisputils

echo pwd = `pwd`
if [ "a" != "$ANS" ]
then
	$ECHONONL	'Load WISP UTILS [y/n/a/q] ? '
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

	drcs borrow wispicon.ico wisp/common
	cp wispicon.ico wrun.ico
fi


echo
echo =================== WISP WPROC ===========================================
echo
echo cd $SOURCEDIR/wproc
cd $SOURCEDIR/wproc

echo pwd = `pwd`
if [ "a" != "$ANS" ]
then
	$ECHONONL	'Load WISP WPROC [y/n/a/q] ? '
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

	chmod +w wproc.umf

	drcs borrow wproc.txt wisp/doc
fi

echo
echo =================== WISP AMU ===========================================
echo
echo cd $SOURCEDIR/amu
cd $SOURCEDIR/amu

echo pwd = `pwd`
if [ "a" != "$ANS" ]
then
	$ECHONONL	'Load WISP AMU [y/n/a/q] ? '
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
echo =================== WISP COSTAR ===========================================
echo
echo cd $SOURCEDIR/costar
cd $SOURCEDIR/costar

echo pwd = `pwd`
if [ "a" != "$ANS" ]
then
	$ECHONONL	'Load WISP COSTAR [y/n/a/q] ? '
	read ANS
fi
if [ "q" = "$ANS" ]
then
	echo 'bldsrckit.sh: aborted.'
	exit 1
fi
if [ "y" = "${ANS}" -o "a" = "${ANS}" ]
then
echo	Loading WISP COSTAR
	drcs unload wisp/costar
fi

echo
echo =================== KCSI ===========================================
echo
echo cd $SOURCEDIR/kcsi
cd $SOURCEDIR/kcsi

echo pwd = `pwd`
if [ "a" != "$ANS" ]
then
	$ECHONONL	'Load KCSI [y/n/a/q] ? '
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
	cd $SOURCEDIR/kcsi
	drcs unload wisp/kcsi

	cd $SOURCEDIR/kcsi/create
	drcs unload wisp/kcsi/create
	drcs unload wisp/kcsi/common
	drcs borrow createntsetup.txt wisp/kcsi/create
	chmod +w *.umf *.mak

	cd $SOURCEDIR/kcsi/crid
	drcs unload wisp/kcsi/crid
	drcs unload wisp/kcsi/common
	drcs borrow cridntsetup.txt wisp/kcsi/crid
	chmod +w *.umf *.mak

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

find . -name '*.dsp' -print | xargs chmod +w
#
#	Create a file version list
#
echo cd $SOURCEDIR
cd $SOURCEDIR
VERSIONS=Versions.lis

echo "# Versions for $WISPXXXX on " `date` > $VERSIONS

for i in $WISP_PROJ_LIST
do
	echo Versions for $i
	drcs vers $i >>$VERSIONS
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
#	Revision 1.47  2001-10-09 16:24:24-04  gsl
#	wproc.lis -> wproc.txt
#	Remove MSDOS
#
#	Revision 1.46  2001-10-09 16:15:04-04  gsl
#	Add wisp_install_unix.txt
#
#	Revision 1.45  2001-09-24 12:45:43-04  gsl
#	Remove kcsi/common dir
#
#	Revision 1.44  2001-09-21 09:25:14-04  gsl
#	Put $WISPXXXX and date into Versions.lis file
#
#	Revision 1.43  1999-09-15 09:31:17-04  gsl
#	Add copy wispicon.ico to wrun.ico
#
#	Revision 1.42  1999-06-08 09:08:48-04  gsl
#	Add wispicon.ico to wisp/acu
#
#	Revision 1.41  1999-03-03 18:14:01-05  gsl
#	Remove cp xxx.umf to Makefile as Makefile is not used
#	Added chmod +w to some of the umf files that need to change on HP
#
#	Revision 1.40  1999-03-01 18:37:41-05  gsl
#	add nttelnet.txt to etc
#
#	Revision 1.39  1999-02-12 14:43:53-05  gsl
#	fix the default for parentdir
#
#	Revision 1.38  1999-02-12 14:34:49-05  gsl
#	Change the "echo -n" with $ECHONONL and add -p to mkdir commands
#
#	Revision 1.37  1999-01-12 12:15:38-05  gsl
#	Make writable makefiles that need to be modified for curses
#
#	Revision 1.36  1998-06-19 10:00:04-04  gsl
#	Add costar to source kit
#	Make test makefiles writable
#
#	Revision 1.35  1998-04-24 15:48:50-04  gsl
#	Update for MSVC++ 5 for NT
#
#	Revision 1.34  1998-04-23 17:09:01-04  gsl
#	Update for MSVC++ 5
#
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
