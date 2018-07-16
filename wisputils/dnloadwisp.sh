#!/bin/sh
#	dnloadwisp.sh
#
#	This routine can only be run from hammer.  It builds the WISP
#	source kit.
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

SOURCEDIR=`pwd`

echo mkdir wisp video
mkdir wisp video

echo cd $SOURCEDIR/video
cd $SOURCEDIR/video

for vdir in lib test cap
do
	echo mkdir $vdir $vdir/src
	mkdir $vdir $vdir/src
done

echo cd $SOURCEDIR/wisp
cd $SOURCEDIR/wisp

for wdir in acu ede etc lib mf msdos port printq proctran test tran utils
do
	echo mkdir $wdir $wdir/src
	mkdir $wdir $wdir/src
done

#
#	Down load all the SCS files into the source kit.
#

echo ' '
echo 'Your choices for each project/component are y=yes, n=no, a=all or q=quit'
echo ' '

#===================== VIDEO LIB ===========================================

echo cd $SOURCEDIR/video/lib/src
cd $SOURCEDIR/video/lib/src

echo pwd = `pwd`
if [ "a" != "$ANS" ]
then
	echo -n	'Load VIDEO LIB [y/n/a/q] ? '
	read ANS
fi
if [ "q" = "$ANS" ]
then
	echo 'dnloadwisp.sh: aborted.'
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
	dcp -c -v 'scalos::scs$video:[common.master.include]*.*' .
	dcp -c -v 'scalos::scs$video:[common.qahold.source]*.*' .
	dcp -c -v 'scalos::scs$video:[common.qahold.include]*.*' .
fi

#===================== VIDEO TEST ===========================================

echo cd $SOURCEDIR/video/test/src
cd $SOURCEDIR/video/test/src

echo pwd = `pwd`
if [ "a" != "$ANS" ]
then
	echo -n	'Load VIDEO TEST [y/n/a/q] ? '
	read ANS
fi
if [ "q" = "$ANS" ]
then
	echo 'dnloadwisp.sh: aborted.'
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
fi

#===================== VIDEO CAP ===========================================

echo cd $SOURCEDIR/video/cap/src
cd $SOURCEDIR/video/cap/src

echo pwd = `pwd`
if [ "a" != "$ANS" ]
then
	echo -n	'Load VIDEO CAP [y/n/a/q] ? '
	read ANS
fi
if [ "q" = "$ANS" ]
then
	echo 'dnloadwisp.sh: aborted.'
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

#===================== WISP ACUCOBOL =======================================

echo cd $SOURCEDIR/wisp/acu/src
cd $SOURCEDIR/wisp/acu/src

echo pwd = `pwd`
if [ "a" != "$ANS" ]
then
	echo -n	'Load WISP ACUCOBOL [y/n/a/q] ? '
	read ANS
fi
if [ "q" = "$ANS" ]
then
	echo 'dnloadwisp.sh: aborted.'
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
fi

#===================== WISP MF ===========================================

echo cd $SOURCEDIR/wisp/mf/src
cd $SOURCEDIR/wisp/mf/src

echo pwd = `pwd`
if [ "a" != "$ANS" ]
then
	echo -n	'Load WISP MF [y/n/a/q] ? '
	read ANS
fi
if [ "q" = "$ANS" ]
then
	echo 'dnloadwisp.sh: aborted.'
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
fi

#===================== WISP PORT ===========================================

echo cd $SOURCEDIR/wisp/port/src
cd $SOURCEDIR/wisp/port/src

echo pwd = `pwd`
if [ "a" != "$ANS" ]
then
	echo -n	'Load WISP PORT [y/n/a/q] ? '
	read ANS
fi
if [ "q" = "$ANS" ]
then
	echo 'dnloadwisp.sh: aborted.'
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
fi

#===================== WISP EDE ===========================================

echo cd $SOURCEDIR/wisp/ede/src
cd $SOURCEDIR/wisp/ede/src

echo pwd = `pwd`
if [ "a" != "$ANS" ]
then
	echo -n	'Load WISP EDE [y/n/a/q] ? '
	read ANS
fi
if [ "q" = "$ANS" ]
then
	echo 'dnloadwisp.sh: aborted.'
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
	dcp -c -v 'scalos::scs$wisp:[common.master.include]*.*' .
	dcp -c -v 'scalos::scs$wisp:[common.qahold.source]*.*' .
	dcp -c -v 'scalos::scs$wisp:[common.qahold.include]*.*' .
fi

#===================== WISP ETC ===========================================

echo cd $SOURCEDIR/wisp/etc/src
cd $SOURCEDIR/wisp/etc/src

echo pwd = `pwd`
if [ "a" != "$ANS" ]
then
	echo -n	'Load WISP ETC [y/n/a/q] ? '
	read ANS
fi
if [ "q" = "$ANS" ]
then
	echo 'dnloadwisp.sh: aborted.'
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
fi

#===================== WISP LIB ===========================================

echo cd $SOURCEDIR/wisp/lib/src
cd $SOURCEDIR/wisp/lib/src

echo pwd = `pwd`
if [ "a" != "$ANS" ]
then
	echo -n	'Load WISP LIB [y/n/a/q] ? '
	read ANS
fi
if [ "q" = "$ANS" ]
then
	echo 'dnloadwisp.sh: aborted.'
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
	dcp -c -v 'scalos::scs$wisp:[common.master.include]*.*' .
	dcp -c -v 'scalos::scs$wisp:[common.qahold.source]*.*' .
	dcp -c -v 'scalos::scs$wisp:[common.qahold.include]*.*' .
fi

#===================== WISP PROCTRAN ========================================

echo cd $SOURCEDIR/wisp/proctran/src
cd $SOURCEDIR/wisp/proctran/src

echo pwd = `pwd`
if [ "a" != "$ANS" ]
then
	echo -n	'Load WISP PROCTRAN [y/n/a/q] ? '
	read ANS
fi
if [ "q" = "$ANS" ]
then
	echo 'dnloadwisp.sh: aborted.'
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
	dcp -c -v 'scalos::scs$wisp:[common.master.include]*.*' .
	dcp -c -v 'scalos::scs$wisp:[common.qahold.source]*.*' .
	dcp -c -v 'scalos::scs$wisp:[common.qahold.include]*.*' .
fi

#===================== WISP TEST ===========================================

echo cd $SOURCEDIR/wisp/test/src
cd $SOURCEDIR/wisp/test/src

echo pwd = `pwd`
if [ "a" != "$ANS" ]
then
	echo -n	'Load WISP SAMPLE [y/n/a/q] ? '
	read ANS
fi
if [ "q" = "$ANS" ]
then
	echo 'dnloadwisp.sh: aborted.'
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

	dcp -c -v 'scalos::scs$wisp:[common.master.source]*.*' .
	dcp -c -v 'scalos::scs$wisp:[common.master.include]*.*' .
	dcp -c -v 'scalos::scs$wisp:[common.qahold.source]*.*' .
	dcp -c -v 'scalos::scs$wisp:[common.qahold.include]*.*' .
fi

#===================== WISP TRAN ===========================================

echo cd $SOURCEDIR/wisp/tran/src
cd $SOURCEDIR/wisp/tran/src

echo pwd = `pwd`
if [ "a" != "$ANS" ]
then
	echo -n	'Load WISP TRAN [y/n/a/q] ? '
	read ANS
fi
if [ "q" = "$ANS" ]
then
	echo 'dnloadwisp.sh: aborted.'
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
	dcp -c -v 'scalos::scs$wisp:[common.master.include]*.*' .
	dcp -c -v 'scalos::scs$wisp:[common.qahold.source]*.*' .
	dcp -c -v 'scalos::scs$wisp:[common.qahold.include]*.*' .
fi

#===================== WISP UTILS ===========================================

echo cd $SOURCEDIR/wisp/utils/src
cd $SOURCEDIR/wisp/utils/src

echo pwd = `pwd`
if [ "a" != "$ANS" ]
then
	echo -n	'Load WISP UTILS [y/n/a/q] ? '
	read ANS
fi
if [ "q" = "$ANS" ]
then
	echo 'dnloadwisp.sh: aborted.'
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

fi

#===================== WISP PRINTQ ==========================================

echo cd $SOURCEDIR/wisp/printq/src
cd $SOURCEDIR/wisp/printq/src

echo pwd = `pwd`
if [ "a" != "$ANS" ]
then
	echo -n	'Load PRINTQ UNIX [y/n/a/q] ? '
	read ANS
fi
if [ "q" = "$ANS" ]
then
	echo 'dnloadwisp.sh: aborted.'
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
fi

#===================== WISP MSDOS ===========================================

echo cd $SOURCEDIR/wisp/msdos/src
cd $SOURCEDIR/wisp/msdos/src

echo pwd = `pwd`
if [ "a" != "$ANS" ]
then
	echo -n	'Load WISP MSDOS [y/n/a/q] ? '
	read ANS
fi
if [ "q" = "$ANS" ]
then
	echo 'dnloadwisp.sh: aborted.'
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

#===================== Down Loading Complete ===============================

echo
echo	Downloading is complete.
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
	echo 'dnloadwisp.sh: aborted.'
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

echo "Removing .sh from file name of selected commands."
echo cd $SOURCEDIR/wisp/port/src
cd $SOURCEDIR/wisp/port/src
for cmd in cacbl doumf echon go lc mkpath search sumnums to version which wver
do
	echo "Moving $cmd.sh to $cmd"
	mv $cmd.sh $cmd
done

echo "Copying mvold.sh & rmold.sh to $SOURCEDIR"
cp mvold.sh rmold.sh $SOURCEDIR

echo " "
echo "Getting printq.doc from SCALOS."
echo cd $SOURCEDIR/wisp/etc/src
cd $SOURCEDIR/wisp/etc/src
dcp -c -v 'scalos::scs$wisp:[doc.master.source]printq.doc' .
dcp -c -v 'scalos::scs$wisp:[doc.qahold.source]printq.doc' .
echo "Moving printq.doc to PRINTQDOC"
mv printq.doc PRINTQDOC

echo 
echo "You must get the current release notes (WISP  DOC  V39X_RELNOTES.LIS)"
echo "and copy them to a $SOURCEDIR/wisp/etc/src/RELNOTES file."
echo 
echo 
echo DONE.
echo 
exit
