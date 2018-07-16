#!/bin/sh
#/************************************************************************/
#/*                                                                      */
#/*              Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993        */
#/*      An unpublished work of International Digital Scientific Inc.    */
#/*                         All rights reserved.                         */
#/*                                                                      */
#/************************************************************************/
#
#
#       File:           bldunisrc.sh
#
#       Function:       This script builds the Unique source kit
#
#       Desciption:     This routine can only be run from hammer or raptor.  
#                       It builds the Unique source kit.
#                       It uses dcp to download all the relevent files
#                       from SCALOS to HAMMER or RAPTOR.  It copies directly out
#                       of the SCS directories.
#
#                       - build the directories
#                       - download each component (MASTER then QAHOLD)
#                       - copy the ".umf" files to "Makefile"
#                       - video include files  --> src/include/v
#                       - sample config files have ".unix" extension removed
#                       - all ".obj" and ".com" files are deleted
#                       - all long files (15+ chars are removed)
#                       - all ".sh" files have x bit set (chmod +x)
#
#       Warning:        If the SCS directory structures change or new ones
#                       are added then this routine must be updated.
#
#       History:        10/28/93        written, based on bldsrckit.sh by JEC
#

# determine how to suppress newline on echo command
tst=`echo -n "hi there"`
if [ "$tst" = "-n hi there" ]
then
    n=''
    c='\c'
else
    n='-n'
    c=''
fi

echo This script will build the Unique source kit.
echo
echo It creates all the directory needed and then uses dcp to copy all the 
echo source files from their SCS directories on SCALOS.
echo
echo $n 'Do you wish to continue [y/n] ? ' "$c"
read ANS
if [ "y" != "${ANS}" ]
then
echo    exiting
	exit
else
echo    continuing...
fi
echo $n "Enter parent directory [$UNIQUE] ? $c"
read ANS
if [ "$ANS" = "" ]
then
   cd $UNIQUE
else
   cd $ANS
fi
PARENTDIR=`pwd`
echo pwd = `pwd`
echo
echo $n 'Enter the version number (ie, 3.02 = 302) ? '"$c"
read VER
ANS=source.v$VER
#
#       Make all the directories needed
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

echo mkdir port printq videolib lib include include/v 
for i in  port printq videolib lib include include/v 
do
        test -d $i || mkdir $i
done

#
#       Down load all the SCS files into the source kit.
#

echo ' '
echo 'Your choices for each project/component are y=yes, n=no, a=all or q=quit'
echo ' '
echo
echo ================== WISP PRINTQ ==========================================
echo
echo cd $SOURCEDIR/printq
cd $SOURCEDIR/printq

echo pwd = `pwd`
if [ "a" != "$ANS" ]
then
        echo $n 'Load PRINTQ UNIX [y/n/a/q] ? '"$c"
        read ANS
fi
if [ "q" = "$ANS" ]
then
        echo 'bldunisrc.sh: aborted.'
        exit 1
fi
if [ "y" = "${ANS}" -o "a" = "${ANS}" ]
then
echo    Loading PRINTQ UNIX
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

        dcp -c -v 'scalos::scs$wisp:[lib.master.source]platsubs.c' .
        dcp -c -v 'scalos::scs$wisp:[lib.qahold.source]platsubs.c' .
        dcp -c -v 'scalos::scs$wisp:[lib.master.source]wlickey.c' .
        dcp -c -v 'scalos::scs$wisp:[lib.qahold.source]wlickey.c' .
        dcp -c -v 'scalos::scs$wisp:[lib.master.source]idsisubs.c' .
        dcp -c -v 'scalos::scs$wisp:[lib.qahold.source]idsisubs.c' .
        dcp -c -v 'scalos::scs$wisp:[lib.master.source]fexists.c' .
        dcp -c -v 'scalos::scs$wisp:[lib.qahold.source]fexists.c' .

        dcp -c -v 'scalos::scs$wisp:[utils.master.source]wlicense.c' ./ulicense.c
        dcp -c -v 'scalos::scs$wisp:[utils.qahold.source]wlicense.c' ./ulicense.c
        dcp -c -v 'scalos::scs$wisp:[utils.master.source]prompt.c' .
        dcp -c -v 'scalos::scs$wisp:[utils.qahold.source]prompt.c' .
	dcp -c -v 'scalos::scs$wisp:[utils.master.build]utils.umf' .
	dcp -c -v 'scalos::scs$wisp:[utils.qahold.build]utils.umf' .

        dcp -c -v 'scalos::scs$wisp:[lib.master.include]*.*' .
        dcp -c -v 'scalos::scs$wisp:[lib.qahold.include]*.*' .
        dcp -c -v 'scalos::scs$wisp:[common.master.include]*.*' .
        dcp -c -v 'scalos::scs$wisp:[common.qahold.include]*.*' .


        cp unique.umf Makefile
fi
echo
echo ==================== VIDEO LIB ===========================================
echo
echo cd $SOURCEDIR/videolib
cd $SOURCEDIR/videolib

echo pwd = `pwd`
if [ "a" != "$ANS" ]
then
        echo $n 'Load VIDEO LIB [y/n/a/q] ? '"$c"
        read ANS
fi
if [ "q" = "$ANS" ]
then
        echo 'bldunisrc.sh: aborted.'
        exit 1
fi
if [ "y" = "${ANS}" -o "a" = "${ANS}" ]
then
echo    Loading VIDEO LIB
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
echo ================== WISP PORT ===========================================
echo
echo cd $SOURCEDIR/port
cd $SOURCEDIR/port

echo pwd = `pwd`
if [ "a" != "$ANS" ]
then
        echo $n 'Load WISP PORT [y/n/a/q] ? '"$c"
        read ANS
fi
if [ "q" = "$ANS" ]
then
        echo 'bldsrckit.sh: aborted.'
        exit 1
fi
if [ "y" = "${ANS}" -o "a" = "${ANS}" ]
then
echo    Loading WISP PORT
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
echo ==================== VIDEO INCLUDE =====================================
echo
echo cd $SOURCEDIR/include/v
cd $SOURCEDIR/include/v
echo Loading VIDEO include headers
dcp -c -v 'scalos::scs$video:[lib.master.include]*.*' .
dcp -c -v 'scalos::scs$video:[lib.qahold.include]*.*' .
dcp -c -v 'scalos::scs$video:[common.master.include]*.*' .
dcp -c -v 'scalos::scs$video:[common.qahold.include]*.*' .

cd $SOURCEDIR/printq
sed 's/\$(WISP)/$(UNIQUE)/g' <Makefile >maketmp
sed 's/\${WISP}/${UNIQUE}/g' <maketmp >Makefile
rm maketmp
cd $SOURCEDIR/videolib
sed 's/$(WISP)/$(UNIQUE)/g' <Makefile >maketmp
sed 's/${WISP}/${UNIQUE}/g' <maketmp >Makefile
rm maketmp
cd $SOURCEDIR/port
sed 's/$(WISP)/$(UNIQUE)/g' <make.include >maketmp
sed 's/${WISP}/${UNIQUE}/g' <maketmp >make.include
sed 's/$(WISP)/$(UNIQUE)/g' <Makefile >maketmp
sed 's/${WISP}/${UNIQUE}/g' <maketmp >Makefile
sed 's/all:.*/all: videolib printq/g' <Makefile >maketmp
sed 's/printq:.*/printq: videolib/g' <maketmp >Makefile
cd $SOURCEDIR/printq
sed "s/VERSION/$VER/g" <Makefile >zzz
mv zzz Makefile

ln -s $SOURCEDIR $PARENTDIR/src
find $SOURCEDIR -name '*.com' -print|xargs rm
cd $SOURCEDIR/..

find src -print|cpio -ocvB|compress -c >../uni${VER}.cpio.Z

