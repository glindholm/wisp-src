#!/bin/sh
########################################################################
#
#	Copyright (c) Shell Stream Software LLC, All rights reserved.
########################################################################
#
#
#       wac     WISP and COMPILE
#
#       Usage:  wac (filepath) (objectdir) (debug)
#
#       This script is used by VSEDIT to WISP and COMPILE a COBOL
#       program.  It can also be used as a general script for doing
#       the above.  It first must be customized for the users
#       environment, see below.
#
#       filepath        The file path of the file to be translated
#                       by WISP.  It can be an absolute or relative
#                       path. It can include an extension, if no
#                       extension then ".wcb" is assumed by WISP.
#
#       objectdir       The directory to place the final COBOL object
#                       files. If ommitted then "." is assumed.  If a
#                       relative path is given it is taken relative to
#                       $filepath.
#
#	debug		The keyword "debug" indicates that the the program
#			should be compiled for debug testing.  This is an
#			OPTIONAL parameter.
#
#
#
#       NOTES:
#		1) This file does NOT redirect stdout or stderr as this
#		   will be done by VSEDIT.
#
#		2) This script may be run directly as long as the
#		   required parameters are passed from the commad line -
#
#		   wac.sh (source filepath) (object directory) (debug)
#
#
########################################################################
#       The following variables must be configured by the user:
#
#       WISPTRAN        This name of the wisp translator
#	WISPTYPE  	This identifies the type of cobol used 
#			with WISP
#       WISPFLAGS       Flags to use with WISP
#
#       COBOL           Name of the COBOL compiler
#       COBFLAGS        Flags to use with the COBOL compiler
#       DEBUG_COBFLAGS  Flags to use with the COBOL compiler when 
#	 		requesting debug test mode
#
#       OBJFILE         The name of the object produced by the COBOL
#                       compiler.  This is used to compare against
#                       TARGET to see if the file must be moved after
#                       it is compiled.
#
#       TARGET          The final name of the COBOL object
#       IDYTARGET       The Micro Focus debug index file to relate
#			the cobol object and source .cob file
#       COBTARGET       The name of the COBOL source file moved to the
#			target object directory for Micro Focus debug
#
########################################################################
#       Other variables used in this script:
#
#       basedir         The directory where the file resides.  This is
#                       extracted from filepath and may be "." if
#                       filepath contains no directory path.
#       filename_ext    The filename portion of filepath including
#                       the extension.
#       filename        The filename without an extension.
#       FILENAME        The uppercase filename.
#	DEBUG		The third argument returned from the command
#			line to define compile in debug test mode
#
########################################################################

commandline="$0 $*"

case $# in
2)      ;;
3)	;;
*)      echo "WISP/compile script -";
	echo "Usage: wac (source filepath) (object directory) (debug)";
	exit 1
esac

filepath=$1
objectdir=$2
DEBUG=$3

basedir=`dirname $filepath`
filename_ext=`basename $filepath`
filename=`echo $filename_ext | cut -f 1 -d .`
FILENAME=`echo $filename | tr '[a-z]' '[A-Z]'`



#########################################
#                                       #
#       Customize this section          #
#                                       #
#  NOTE: Remove "#"s in front of cobol  #
#        variables for either AcuCobol  #
#        or Micro Focus cobol           #

WISPTRAN=wisp

### *** ACUCOBOL ***
#WISPTYPE="-VACU"
#WISPFLAGS=${WISPTYPE}
#COBOL=ccbl
#OBJFILE=$objectdir/$FILENAME
#TARGET=$OBJFILE
#COBFLAGS="-Da4 -Lwo \@.lst -o $OBJFILE"
#DEBUG_COBFLAGS="-Gd -Za -Da4 -Lwo \@.lst -o $OBJFILE"

### *** Micro Focus ***
#WISPTYPE="-VMF"
#WISPFLAGS=${WISPTYPE}
#COBOL=cob
#IDYFILE=./$filename.idy
#COBFILE=./$filename.cob
#COBFLAGS="-i -P"
#DEBUG_COBFLAGS="-i -a -P -C BOUND"
#OBJFILE=./$filename.int
#TARGET=$objectdir/$FILENAME.int
#IDYTARGET=$objectdir/$FILENAME.idy
#COBTARGET=$objectdir/$filename.cob

#       End of Customized section       #
#                                       #
#########################################

if [ -z "${WISPTRAN}" ]
then
	echo "# NO WISP defined in wac.sh script!"
	echo "# Edit the wac.sh script to define WISP translator"
	echo "# " `date`
	exit 1
fi

if [ -z "$COBOL" ]
then
	echo "# NO COBOL defined in wac.sh script!"
	echo "# Edit the wac.sh script to define COBOL type"
	echo "# " `date`
	exit 1
fi

if [ -z "$OBJFILE" ]
then
	echo "# NO OBJECT file defined in wac.sh script!"
	echo "# Edit the wac.sh script to define OBJECT file"
	echo "# " `date`
	exit 1
fi

echo "#" `date`
echo "# $commandline"

if [ ! -d $basedir ]
then
        echo "# Directory $basedir does not exist!"
        echo "#" `date`
        exit 1
fi

echo
echo "cd $basedir; pwd"
cd $basedir; pwd


if [ ! -d $objectdir ]
then
        echo
        echo "# Creating object directory $objectdir"
        echo mkdir $objectdir
        mkdir $objectdir
        if [ ! -d $objectdir ]
        then
                echo
                echo "# Unable to create $objectdir!"
                echo "#" `date`
                exit 1
        fi
fi


if [ -f $OBJFILE ]
then
        echo
        echo "# REMOVING old object"
        echo rm -f $OBJFILE
        rm -f $OBJFILE
fi

if [ $IDYFILE ]
then
	if [ -f $IDYFILE ]
	then
       		echo
        	echo "# REMOVING old idy debug file"
        	echo rm -f $IDYFILE
        	rm -f $IDYFILE
	fi
fi

if [ -f $TARGET ]
then
        echo
        echo "# REMOVING old target"
        echo rm -f $TARGET
        rm -f $TARGET
fi

if [ $IDYTARGET ]
then
	if [ -f $IDYTARGET ]
	then
        	echo
        	echo "# REMOVING old idy debug target"
        	echo rm -f $IDYTARGET
        	rm -f $IDYTARGET
	fi
fi

if [ $COBTARGET ]
then
	if [ -f $COBTARGET ]
	then
        	echo
        	echo "# REMOVING old cob debug target"
        	echo rm -f $COBTARGET
        	rm -f $COBTARGET
	fi
fi

echo
echo "# WISPING $filename_ext"
echo ${WISPTRAN} ${WISPFLAGS} $filename_ext
${WISPTRAN} ${WISPFLAGS} $filename_ext
rc=$?
if [ "0" != "$rc" ]
then
        echo
        echo "# WISP failed with exit code =" $rc
        echo "# COBOL compile aborted!"
        echo "#" `date`
        exit 1
fi


echo
echo "# COMPILING $filename.cob"
if [ "$DEBUG" = "debug" ]
then
	echo $COBOL $DEBUG_COBFLAGS $filename.cob
	$COBOL $DEBUG_COBFLAGS $filename.cob
else
	echo $COBOL $COBFLAGS $filename.cob
	$COBOL $COBFLAGS $filename.cob
fi
rc=$?
echo "rc=$rc"



if [ ! -f $OBJFILE ]
then
        echo
        echo "# COBOL object file $OBJFILE not found!"
        echo "#" `date`
        exit 1
fi

if [ ! -s $OBJFILE ]
then
        echo
        echo "# COBOL object file $OBJFILE is zero length!"
        echo "#" `date`
        exit 1
fi

if [ "$TARGET" != "$OBJFILE" ]
then
        echo
        echo "# Moving COBOL object file to $objectdir"
        echo mv $OBJFILE $TARGET
        mv $OBJFILE $TARGET
	if [ "$DEBUG" = "debug" -a  $IDYFILE ]
	then
        	echo "# Moving COBOL DEBUG files to $objectdir"
		if [ ! -f $IDYTARGET ]
		then
			echo mv $IDYFILE $IDYTARGET
			mv $IDYFILE $IDYTARGET
		fi
		if [ ! -f $COBTARGET ]
		then
			echo cp $COBFILE $COBTARGET
			cp $COBFILE $COBTARGET
		fi
	fi
fi

echo
echo "#" `date`
exit $rc
