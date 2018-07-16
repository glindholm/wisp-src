#!/bin/ksh
#
#	Copyright (c) 1995 DevTech Migrations, All rights reserved.
#	$Id:$
#
#
#	File:		marksrckit.sh
#
#	Function:	This script sets the RCS state for all the
#			source files in WISP.
#
#	Desciption:	This routine can only be run from zaphod.  
#			It uses the "drcs state" command to set
#			the states for each project.
#

proj_list="
video/cap
video/ivs
video/lib
video/test
wisp/acucobol
wisp/amu
wisp/common
wisp/ede
wisp/etc
wisp/ivs
wisp/lib
wisp/menudemo
wisp/mf
wisp/msdos
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
echo "It is run before bldsrckit.sh to set the RCS states of all the"
echo "files in WISP to the new version number (e.g. V3_3_15)."
echo
echo It uses DRCS so can only be run from ZAPHOD
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

#
#	Check for locked files
#
echo
echo Checking for locked files...
echo
for the_proj in $proj_list
do
	drcs out $the_proj
done
echo
echo If there are any files listed above they should be logged in
echo before continuing.
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

#
#	Get the version number to use
#
echo
echo -n	"Enter the state to use (e.g. V3_3_15) ? "
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
for the_proj in $proj_list
do
	echo Setting state for project $the_proj
	drcs state $the_state $the_proj
done

echo 
echo 
echo DONE.
echo 
exit 0
