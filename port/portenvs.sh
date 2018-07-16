#!/bin/ksh
#	FILE:	portenvs.sh
#
#	USAGE:	$ cd ../wisp/src/port
#		$ portenvs.sh
#
#	Set the environment variables needed to do the WISP port.
#
#
SAVEPWD=`pwd`
cd ..
export WISPDIR=`pwd`
cd ..
export WISP=`pwd`
cd $WISPDIR/port
if [ $SAVEPWD != $WISPDIR/port ]
then
	echo ERROR must be run from wisp/src/port directory
	exit 1
fi

echo WISP=$WISP
echo WISPDIR=$WISPDIR
echo ACUDIR=$ACUDIR
if [ ! -d $ACUDIR ]
then
	echo ACUDIR not found
	exit 1
fi
echo COBDIR=$COBDIR
if [ ! -d $COBDIR ]
then
	echo COBDIR not found
	exit 1
fi
echo SHELL=$SHELL
echo
echo Switching to new shell for WISP PORT
echo
export PS1="WISP PORT `hostname` $ "
$SHELL
