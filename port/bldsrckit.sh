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
#	File:		bldsrckit.sh
#
#	Function:	This script builds the WISP source kit
#
#	Desciption:	This routine can only be run from hammer.  
#			It builds the WISP source kit.
#			It uses dcp to download all the relevent files
#			from SCALOS to HAMMER.  It copies directly out
#			of the SCS directories.
#
#			- build the directories
#			- download each component (MASTER then QAHOLD)
#			- copy the ".umf" files to "Makefile"
#			- video include files  --> src/include/v
#			- sample config files have ".unix" extension removed
#			- all ".obj" and ".com" files are deleted
#			- all long files (15+ chars are removed)
#			- all ".sh" files have x bit set (chmod +x)
#
#	Warning:	If the SCS directory structures change or new ones
#			are added then this routine must be updated.
#
#	History:	06/04/92	Written. GSL
#			06/09/92	Remove src/bin directory. GSL
#			06/09/92	Added copy of .umf to Makefile. GSL
#			06/30/92	Split src/test into src/testacu & src/testmf. GSL
#			07/07/92	Added wisp common include to printq. GSL
#

echo This script will build WISP source kit.
echo
echo It creates all the directory needed and then uses dcp to copy all the 
echo source files from their SCS directories on SCALOS.
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
echo
echo -n	'Enter parent directory [/usr2/wisp] ? '
read ANS
if [ "$ANS" = "" ]
then
	cd /usr2/wisp
else
	cd $ANS
fi
PARENTDIR=`pwd`
echo pwd = `pwd`
echo
echo -n	'Enter name of save dir (e.g. source.v32a) ? '
read ANS

#
#	Make all the directories needed
#

echo mkdir $ANS
mkdir $ANS

echo cd $ANS
cd $ANS

echo mkdir src
mkdir src

echo cd src
cd src

SOURCEDIR=`pwd`

echo mkdir acu ede etc include mf msdos port printq proctran 
mkdir acu ede etc include mf msdos port printq proctran

echo mkdir videolib videotest videocap wisplib wisptran wisputils
mkdir videolib videotest videocap wisplib wisptran wisputils

echo mkdir lib lib/x include/v testacu testmf
mkdir lib lib/x include/v testacu testmf

#
#	Down load all the SCS files into the source kit.
#

echo ' '
echo 'Your choices for each project/component are y=yes, n=no, a=all or q=quit'
echo ' '

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
	dcp -c -v 'scalos::scs$video:[lib.master.source]*.*' .
	dcp -c -v 'scalos::scs$video:[lib.master.include]*.*' .
	dcp -c -v 'scalos::scs$video:[lib.master.build]*.*' .
	dcp -c -v 'scalos::scs$video:[lib.master.misc]*.*' .
	dcp -c -v 'scalos::scs$video:[lib.qahold.source]*.*' .
	dcp -c -v 'scalos::scs$video:[lib.qahold.include]*.*' .
	dcp -c -v 'scalos::scs$video:[lib.qahold.build]*.*' .
	dcp -c -v 'scalos::scs$video:[lib.qahold.misc]*.*' .

	dcp -c -v 'scalos::scs$video:[common.master.source]*.*' .
	dcp -c -v 'scalos::scs$video:[common.qahold.source]*.*' .
	dcp -c -v 'scalos::scs$video:[common.master.include]*.*' .
	dcp -c -v 'scalos::scs$video:[common.qahold.include]*.*' .

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
	dcp -c -v 'scalos::scs$video:[test.master.source]*.*' .
	dcp -c -v 'scalos::scs$video:[test.master.include]*.*' .
	dcp -c -v 'scalos::scs$video:[test.master.build]*.*' .
	dcp -c -v 'scalos::scs$video:[test.master.misc]*.*' .
	dcp -c -v 'scalos::scs$video:[test.qahold.source]*.*' .
	dcp -c -v 'scalos::scs$video:[test.qahold.include]*.*' .
	dcp -c -v 'scalos::scs$video:[test.qahold.build]*.*' .
	dcp -c -v 'scalos::scs$video:[test.qahold.misc]*.*' .

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
	dcp -c -v 'scalos::scs$video:[cap.master.source]*.*' .
	dcp -c -v 'scalos::scs$video:[cap.master.include]*.*' .
	dcp -c -v 'scalos::scs$video:[cap.master.build]*.*' .
	dcp -c -v 'scalos::scs$video:[cap.master.misc]*.*' .
	dcp -c -v 'scalos::scs$video:[cap.qahold.source]*.*' .
	dcp -c -v 'scalos::scs$video:[cap.qahold.include]*.*' .
	dcp -c -v 'scalos::scs$video:[cap.qahold.build]*.*' .
	dcp -c -v 'scalos::scs$video:[cap.qahold.misc]*.*' .
fi

echo
echo ==================== VIDEO INCLUDE =====================================
echo
echo cd $SOURCEDIR/include/v
cd $SOURCEDIR/include/v
echo Loading VIDEO include headers
dcp -c -v 'scalos::scs$video:[lib.master.include]*.*' .
dcp -c -v 'scalos::scs$video:[lib.qahold.include]*.*' .

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
	dcp -c -v 'scalos::scs$wisp:[acucobol.master.source]*.*' .
	dcp -c -v 'scalos::scs$wisp:[acucobol.master.include]*.*' .
	dcp -c -v 'scalos::scs$wisp:[acucobol.master.build]*.*' .
	dcp -c -v 'scalos::scs$wisp:[acucobol.master.misc]*.*' .
	dcp -c -v 'scalos::scs$wisp:[acucobol.qahold.source]*.*' .
	dcp -c -v 'scalos::scs$wisp:[acucobol.qahold.include]*.*' .
	dcp -c -v 'scalos::scs$wisp:[acucobol.qahold.build]*.*' .
	dcp -c -v 'scalos::scs$wisp:[acucobol.qahold.misc]*.*' .

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
	dcp -c -v 'scalos::scs$wisp:[mf.master.source]*.*' .
	dcp -c -v 'scalos::scs$wisp:[mf.master.include]*.*' .
	dcp -c -v 'scalos::scs$wisp:[mf.master.build]*.*' .
	dcp -c -v 'scalos::scs$wisp:[mf.master.misc]*.*' .
	dcp -c -v 'scalos::scs$wisp:[mf.qahold.source]*.*' .
	dcp -c -v 'scalos::scs$wisp:[mf.qahold.include]*.*' .
	dcp -c -v 'scalos::scs$wisp:[mf.qahold.build]*.*' .
	dcp -c -v 'scalos::scs$wisp:[mf.qahold.misc]*.*' .

	cp wrunmfx.umf Makefile
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
	dcp -c -v 'scalos::scs$wisp:[port.master.source]*.*' .
	dcp -c -v 'scalos::scs$wisp:[port.master.include]*.*' .
	dcp -c -v 'scalos::scs$wisp:[port.master.build]*.*' .
	dcp -c -v 'scalos::scs$wisp:[port.master.misc]*.*' .
	dcp -c -v 'scalos::scs$wisp:[port.qahold.source]*.*' .
	dcp -c -v 'scalos::scs$wisp:[port.qahold.include]*.*' .
	dcp -c -v 'scalos::scs$wisp:[port.qahold.build]*.*' .
	dcp -c -v 'scalos::scs$wisp:[port.qahold.misc]*.*' .

	cp makewisp.umf Makefile
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
	dcp -c -v 'scalos::scs$wisp:[ede.master.source]*.*' .
	dcp -c -v 'scalos::scs$wisp:[ede.master.include]*.*' .
	dcp -c -v 'scalos::scs$wisp:[ede.master.build]*.*' .
	dcp -c -v 'scalos::scs$wisp:[ede.master.misc]*.*' .
	dcp -c -v 'scalos::scs$wisp:[ede.qahold.source]*.*' .
	dcp -c -v 'scalos::scs$wisp:[ede.qahold.include]*.*' .
	dcp -c -v 'scalos::scs$wisp:[ede.qahold.build]*.*' .
	dcp -c -v 'scalos::scs$wisp:[ede.qahold.misc]*.*' .

	dcp -c -v 'scalos::scs$wisp:[menudemo.master.source]*.*' .
	dcp -c -v 'scalos::scs$wisp:[menudemo.master.include]*.*' .
	dcp -c -v 'scalos::scs$wisp:[menudemo.master.build]*.*' .
	dcp -c -v 'scalos::scs$wisp:[menudemo.master.misc]*.*' .
	dcp -c -v 'scalos::scs$wisp:[menudemo.qahold.source]*.*' .
	dcp -c -v 'scalos::scs$wisp:[menudemo.qahold.include]*.*' .
	dcp -c -v 'scalos::scs$wisp:[menudemo.qahold.build]*.*' .
	dcp -c -v 'scalos::scs$wisp:[menudemo.qahold.misc]*.*' .

	dcp -c -v 'scalos::scs$wisp:[common.master.source]*.*' .
	dcp -c -v 'scalos::scs$wisp:[common.qahold.source]*.*' .
	dcp -c -v 'scalos::scs$wisp:[common.master.include]*.*' .
	dcp -c -v 'scalos::scs$wisp:[common.qahold.include]*.*' .

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
	dcp -c -v 'scalos::scs$wisp:[etc.master.source]*.*' .
	dcp -c -v 'scalos::scs$wisp:[etc.master.include]*.*' .
	dcp -c -v 'scalos::scs$wisp:[etc.master.build]*.*' .
	dcp -c -v 'scalos::scs$wisp:[etc.master.misc]*.*' .
	dcp -c -v 'scalos::scs$wisp:[etc.qahold.source]*.*' .
	dcp -c -v 'scalos::scs$wisp:[etc.qahold.include]*.*' .
	dcp -c -v 'scalos::scs$wisp:[etc.qahold.build]*.*' .
	dcp -c -v 'scalos::scs$wisp:[etc.qahold.misc]*.*' .

	for old in *.unix
	do
		new=`basename $old .unix`
		echo "moving $old to $new"
		mv $old $new
	done

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
	dcp -c -v 'scalos::scs$wisp:[lib.master.source]*.*' .
	dcp -c -v 'scalos::scs$wisp:[lib.master.include]*.*' .
	dcp -c -v 'scalos::scs$wisp:[lib.master.build]*.*' .
	dcp -c -v 'scalos::scs$wisp:[lib.master.misc]*.*' .
	dcp -c -v 'scalos::scs$wisp:[lib.qahold.source]*.*' .
	dcp -c -v 'scalos::scs$wisp:[lib.qahold.include]*.*' .
	dcp -c -v 'scalos::scs$wisp:[lib.qahold.build]*.*' .
	dcp -c -v 'scalos::scs$wisp:[lib.qahold.misc]*.*' .

	dcp -c -v 'scalos::scs$wisp:[common.master.source]*.*' .
	dcp -c -v 'scalos::scs$wisp:[common.qahold.source]*.*' .
	dcp -c -v 'scalos::scs$wisp:[common.master.include]*.*' .
	dcp -c -v 'scalos::scs$wisp:[common.qahold.include]*.*' .

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
	dcp -c -v 'scalos::scs$wisp:[proctran.master.source]*.*' .
	dcp -c -v 'scalos::scs$wisp:[proctran.master.include]*.*' .
	dcp -c -v 'scalos::scs$wisp:[proctran.master.build]*.*' .
	dcp -c -v 'scalos::scs$wisp:[proctran.master.misc]*.*' .
	dcp -c -v 'scalos::scs$wisp:[proctran.qahold.source]*.*' .
	dcp -c -v 'scalos::scs$wisp:[proctran.qahold.include]*.*' .
	dcp -c -v 'scalos::scs$wisp:[proctran.qahold.build]*.*' .
	dcp -c -v 'scalos::scs$wisp:[proctran.qahold.misc]*.*' .

	dcp -c -v 'scalos::scs$wisp:[common.master.source]*.*' .
	dcp -c -v 'scalos::scs$wisp:[common.qahold.source]*.*' .

	cp proctran.umf Makefile
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
	dcp -c -v 'scalos::scs$wisp:[sample.master.source]*.*' .
	dcp -c -v 'scalos::scs$wisp:[sample.master.include]*.*' .
	dcp -c -v 'scalos::scs$wisp:[sample.master.build]*.*' .
	dcp -c -v 'scalos::scs$wisp:[sample.master.misc]*.*' .
	dcp -c -v 'scalos::scs$wisp:[sample.qahold.source]*.*' .
	dcp -c -v 'scalos::scs$wisp:[sample.qahold.include]*.*' .
	dcp -c -v 'scalos::scs$wisp:[sample.qahold.build]*.*' .
	dcp -c -v 'scalos::scs$wisp:[sample.qahold.misc]*.*' .
	echo Duplicating testacu into testmf
	cp * ../testmf
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
	dcp -c -v 'scalos::scs$wisp:[tran.master.source]*.*' .
	dcp -c -v 'scalos::scs$wisp:[tran.master.include]*.*' .
	dcp -c -v 'scalos::scs$wisp:[tran.master.build]*.*' .
	dcp -c -v 'scalos::scs$wisp:[tran.master.misc]*.*' .
	dcp -c -v 'scalos::scs$wisp:[tran.qahold.source]*.*' .
	dcp -c -v 'scalos::scs$wisp:[tran.qahold.include]*.*' .
	dcp -c -v 'scalos::scs$wisp:[tran.qahold.build]*.*' .
	dcp -c -v 'scalos::scs$wisp:[tran.qahold.misc]*.*' .

	dcp -c -v 'scalos::scs$wisp:[common.master.source]*.*' .
	dcp -c -v 'scalos::scs$wisp:[common.qahold.source]*.*' .
	dcp -c -v 'scalos::scs$wisp:[common.master.include]*.*' .
	dcp -c -v 'scalos::scs$wisp:[common.qahold.include]*.*' .

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

	dcp -c -v 'scalos::scs$wisp:[utils.master.source]*.*' .
	dcp -c -v 'scalos::scs$wisp:[utils.qahold.source]*.*' .
	dcp -c -v 'scalos::scs$wisp:[utils.master.include]*.*' .
	dcp -c -v 'scalos::scs$wisp:[utils.qahold.include]*.*' .
	dcp -c -v 'scalos::scs$wisp:[utils.master.build]*.*' .
	dcp -c -v 'scalos::scs$wisp:[utils.qahold.build]*.*' .
	dcp -c -v 'scalos::scs$wisp:[utils.master.misc]*.*' .
	dcp -c -v 'scalos::scs$wisp:[utils.qahold.misc]*.*' .

	dcp -c -v 'scalos::scs$wisp:[common.master.include]*.*' .
	dcp -c -v 'scalos::scs$wisp:[common.qahold.include]*.*' .

	cp utils.umf Makefile
fi

echo
echo ================== WISP PRINTQ ==========================================
echo
echo cd $SOURCEDIR/printq
cd $SOURCEDIR/printq

echo pwd = `pwd`
if [ "a" != "$ANS" ]
then
	echo -n	'Load PRINTQ UNIX [y/n/a/q] ? '
	read ANS
fi
if [ "q" = "$ANS" ]
then
	echo 'bldsrckit.sh: aborted.'
	exit 1
fi
if [ "y" = "${ANS}" -o "a" = "${ANS}" ]
then
echo	Loading PRINTQ UNIX
	dcp -c -v 'scalos::scs$printq:[unix.master.source]*.*' .
	dcp -c -v 'scalos::scs$printq:[unix.master.include]*.*' .
	dcp -c -v 'scalos::scs$printq:[unix.master.build]*.*' .
	dcp -c -v 'scalos::scs$printq:[unix.master.misc]*.*' .
	dcp -c -v 'scalos::scs$printq:[unix.qahold.source]*.*' .
	dcp -c -v 'scalos::scs$printq:[unix.qahold.include]*.*' .
	dcp -c -v 'scalos::scs$printq:[unix.qahold.build]*.*' .
	dcp -c -v 'scalos::scs$printq:[unix.qahold.misc]*.*' .

	dcp -c -v 'scalos::scs$wisp:[common.master.include]*.*' .
	dcp -c -v 'scalos::scs$wisp:[common.qahold.include]*.*' .

	cp printq.umf Makefile
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
	dcp -c -v 'scalos::scs$wisp:[msdos.master.source]*.*' .
	dcp -c -v 'scalos::scs$wisp:[msdos.master.include]*.*' .
	dcp -c -v 'scalos::scs$wisp:[msdos.master.build]*.*' .
	dcp -c -v 'scalos::scs$wisp:[msdos.master.misc]*.*' .
	dcp -c -v 'scalos::scs$wisp:[msdos.qahold.source]*.*' .
	dcp -c -v 'scalos::scs$wisp:[msdos.qahold.include]*.*' .
	dcp -c -v 'scalos::scs$wisp:[msdos.qahold.build]*.*' .
	dcp -c -v 'scalos::scs$wisp:[msdos.qahold.misc]*.*' .
fi

echo
echo ================== Down Loading Complete ===============================
echo
echo
echo cd $SOURCEDIR
cd $SOURCEDIR

echo pwd = `pwd`
if [ "a" != "$ANS" ]
then
	echo
	echo -n 'Delete all *.com and *.obj files [y/n/a/q] ? '
	read ANS
fi
if [ "q" = "$ANS" ]
then
	echo 'bldsrckit.sh: aborted.'
	exit 1
fi
if [ "y" = "${ANS}" -o "a" = "${ANS}" ]
then
echo 'find . -name "*.com" -print -exec rm {} \;'
find . -name "*.com" -print -exec rm {} \;
echo 'find . -name "*.obj" -print -exec rm {} \;'
find . -name "*.obj" -print -exec rm {} \;
fi

#
#	Remove all long names greater then 14 characters
#

echo "This routine next removes any file names longer than 14 characters."
echo

for fil in `find . -name "???????????????*" -print`
do
	echo "File <$fil> name too long. Rename?"
	read ans
	if [ "$ans" = "y" ]
	then
		echo "Enter new name:"
		read nam
		bas=`dirname $fil`
		if [ -f $bas/$nam ]
		then
			echo "$bas/$name exists"
			ls $bas
			echo "Enter new name:"
			read nam
		fi
		echo "Rename $fil to $bas/$nam"
		mv $fil $bas/$nam
	else
		echo "Delete it?"
		read ans
		if [ "$ans" = "y" ]
		then
			echo "Delete $fil"
			rm $fil
		else
			echo "Name is still too long."
		fi
	fi
done

#
#	File name cleanup
#

echo " "
echo "Changing mode of all .sh files to 771."
find . -name '*.sh' -exec chmod 771 {} \; -print

#echo "Removing .sh from file name of selected commands."
#echo cd $SOURCEDIR/port
#cd $SOURCEDIR/port
#for cmd in cacbl doumf echon go lc mkpath search sumnums to version which wver
#do
#	echo "Moving $cmd.sh to $cmd"
#	mv $cmd.sh $cmd
#done
#
#echo "Copying mvold.sh & rmold.sh to $SOURCEDIR"
#cp mvold.sh rmold.sh $SOURCEDIR
#
#echo " "
#echo "Getting printq.doc from SCALOS."
#echo cd $SOURCEDIR/etc
#cd $SOURCEDIR/etc
#dcp -c -v 'scalos::scs$wisp:[doc.master.source]printq.doc' .
#dcp -c -v 'scalos::scs$wisp:[doc.qahold.source]printq.doc' .
#echo "Moving printq.doc to PRINTQDOC"
#mv printq.doc PRINTQDOC

echo 
echo "You must get the current release notes (WISP  DOC  V39X_RELNOTES.LIS)"
echo "and copy them to a $SOURCEDIR/etc/RELNOTES file."
echo 
echo 
echo DONE.
echo 
exit
