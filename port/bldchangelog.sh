#!/bin/ksh
#
#	$Id:$
#
#
#	File:		bldchangelog.sh
#
#	Function:	This script creates a change log for WISP since a given date
#
#	Desciption:	This routine can only be run from zigzag.  
#			It uses the "drcs log" command to get a log for each project.
#

proj_list="
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

echo
echo "This script is part of the process of creating a WISP release."
echo "It is run before bldsrckit.sh to get a change log"
echo
echo It uses DRCS so can only be run from ZIGZAG
echo
echo -n	'Do you wish to continue [y/n] ? ' 
read ANS
if [ "y" != "${ANS}" ]
then
	echo	exiting
	exit 0
else
	echo	continuing...
fi

echo 
echo -n 'Enter the starting date (YYYY/MM/DD) ? '
read STARTDATE

if [ -f change.log ]
then
	echo
	echo Removing old change.log
	rm -f change.log
fi

echo
echo Creating new change.log for date=$STARTDATE
echo
echo WISP CHANGE LOG DATE=$STARTDATE 		>change.log

for the_proj in $proj_list
do
	echo "Processing $the_proj"
	echo					>>change.log
	echo "=============================="	>>change.log
	echo "Change log for $the_proj"		>>change.log
	echo "------------------------------"	>>change.log

	drcs log $STARTDATE $the_proj		>>change.log

	echo "------------------------------"	>>change.log
done

echo 
echo 
echo DONE.
echo 
exit 0
