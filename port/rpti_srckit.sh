#
#	Copyright (c) 1995 DevTech Migrations, All rights reserved.
#	$Id:$
#
#
#	File:		rpti_srckit.sh
#
#	RCS:		$Source:$
#
#	Function:	This script builds the RPTI source kit
#
#	Desciption:	This routine can only be run from zaphod.  
#

echo This script will build the RPTI source kit.
echo
echo It creates all the directory needed and then uses DRCS to unload all the 
echo source files from their DRCS directories on ZAPHOD.
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
PARENTDIR=`pwd`
echo
echo -n	"Enter parent directory [$PARENTDIR] ? "
read ANS
if [ "$ANS" != "" ]
then
	cd $ANS
fi
PARENTDIR=`pwd`
echo pwd = `pwd`
echo
echo -n	"Enter name of the distribution dir [rpti] ? "
read ANS
if [ "$ANS" = "" ]
then
	ANS=rpti
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

echo mkdir videolib wisplib wisputils port etc include
for i in   videolib wisplib wisputils port etc include
do
	test -d $i || mkdir $i
done


#
#	Down load all the SCS files into the source kit.
#

# Download All
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
	drcs unload video/common

	cp libvideo.umf Makefile
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

	for old in *.unix
	do
		new=`basename $old .unix`
		echo "moving $old to $new"
		mv $old $new
	done

	for i in `drcs dir wisp/doc | sed -n '/relnotes/p'`
	do
		drcs borrow $i wisp/doc
	done

	drcs borrow qawisp.lis wisp/doc
	drcs borrow qaede.doc wisp/doc
	drcs borrow portunix.lis wisp/doc

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

#	drcs unload wisp/lib

WISPLIB_FILES=" \
	bit_x.c \
	cobrun.h \
	libwisp.umf \
	movebin.h \
	print.c \
	screen.c \
	sub_char.h \
	vdisplay.c \
	vwang.c \
	wdisplay.c \
	wscreen.c \
	wscrn.h \
	wshelp.c \
	wsmove.c \
	wsxio.c \
	"

	for i in $WISPLIB_FILES
	do
		drcs borrow $i wisp/lib
	done
fi

echo
echo =================== WISP COMMON ===========================================
echo
echo cd $SOURCEDIR/include
cd $SOURCEDIR/include

echo pwd = `pwd`
if [ "a" != "$ANS" ]
then
	echo -n	'Load WISP COMMON [y/n/a/q] ? '
	read ANS
fi
if [ "q" = "$ANS" ]
then
	echo 'bldsrckit.sh: aborted.'
	exit 1
fi
if [ "y" = "${ANS}" -o "a" = "${ANS}" ]
then
echo	Loading WISP COMMON

	drcs unload wisp/common

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

#	drcs unload wisp/utils

WISPUTILS_FILES=" \
	display.c \
	wusage.c \
	wsort.c \
	utils.umf \
	"

	for i in $WISPUTILS_FILES
	do
		drcs borrow $i wisp/utils
	done

	cp utils.umf Makefile
fi


echo
echo ================== Down Loading Complete ===============================
echo
echo

#
#	File name cleanup
#

echo " "
echo "Changing mode of all .sh files to 771."
find . -name '*.sh' -print | tee /dev/tty | xargs chmod 771


echo 
echo "The latest release notes [" $latest_relnotes "] have"
echo "been copied to etc/RELNOTES"
echo 
echo 
echo DONE.
echo 
exit
