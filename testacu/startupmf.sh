#!/bin/sh
#
#	File:		startupmf.sh
#
#	Usage:		$ cd .../wisp/src/testmf
#			$ startupmf.sh
#
#	Function:	To setup env variables needed to QA WISP
#			using Micro Focus .
#
QA=`pwd`
WISPDIR=../../wisp.QA
if [ ! -d $WISPDIR ]
then
	echo "**** WISPDIR=$WISPDIR NOT FOUND"
	WISPDIR=/usr/local/wisp
	echo "**** USING WISPDIR=$WISPDIR"
else
	cd $WISPDIR
	WISPDIR=`pwd`
	cd $QA
fi
cd ../..
WISP=`pwd`
cd $QA
WISPCONFIG=$QA/config
COBSW=-F
COBPATH=:$QA
PATH=$WISPDIR/bin:$WISPDIR/cridmfx:$WISPDIR/createmfx:$QA/volrun/onpath:$PATH
export QA WISPCONFIG COBSW COBPATH PATH
echo
echo '**** SETTING UP FOR MICRO FOCUS ****'
echo "WISP       = $WISP"
echo "WISPDIR    = $WISPDIR"
echo "QA         = $QA"
echo "WISPCONFIG = $WISPCONFIG"
echo "COBDIR     = $COBDIR"
echo "COBPATH    = $COBPATH"
echo "COBSW      = $COBSW"
echo "SHELL      = $SHELL"
echo "PATH       = $PATH"

COBVER=$COBDIR/etc/cobver
if [ ! -f $COBVER ]
then
COBVER=$COBDIR/cobver
fi
cat $COBVER
wisp|grep Version

echo
echo Switching to new shell for WISP QA
echo
# PS1="TESTMF `hostname` $ "
PS1="TESTMF `uname -n` $ "
export PS1
$SHELL
