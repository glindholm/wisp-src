#!/bin/sh
#
#	$Id:$
#
#	File:		startupacu.sh
#
#	Usage:		$ cd .../wisp/src/testacu
#			$ startupacu.sh
#
#	Function:	To setup env variables needed to QA WISP
#			using ACUCOBOL.
#
QA=`pwd`
WISPDIR=../QA/wisp
if [ ! -d ${WISPDIR} ]
then
	echo "**** WISPDIR=${WISPDIR} NOT FOUND"
	WISPDIR=/usr/local/wisp
	echo "**** USING WISPDIR=${WISPDIR}"
else
	cd ${WISPDIR}
	WISPDIR=`pwd`
fi
cd ${QA}
cd ..
WISPSRC=`pwd`
cd ${QA}
WISPCONFIG=${QA}/config
A_CONFIG=${WISPCONFIG}/ACUCONFIG
PATH=${WISPDIR}/bin:${WISPDIR}/kcsiacu:${QA}/volrun/onpath:${ACUDIR}/bin:${PATH}
export QA WISPCONFIG A_CONFIG PATH WISPDIR WISPSRC
echo
echo "**** SETTING UP FOR ACUCOBOL ****"
echo "WISPDIR      = ${WISPDIR}"
echo "WISPSRC      = ${WISPSRC}"
echo "QA           = ${QA}"
echo "WISPCONFIG   = ${WISPCONFIG}"
echo "A_CONFIG     = ${A_CONFIG}"
echo "SHELL        = ${SHELL}"
echo "PATH         = ${PATH}"
ccbl -v
wisp|grep Version

# If needed create ${WISPCONFIG} dir 
if [ ! -d ${WISPCONFIG} ]
then
	configacu.sh
fi
wusage set PROGVOL=SOURCE
wusage set PROGLIB=TESTACU
wusage set RUNVOL=SOURCE
wusage set RUNLIB=TESTACU

echo
echo Switching to new shell for WISP QA
echo
#PS1="TESTACU `hostname` $ "
PS1="TESTACU `uname -n` $ "
export PS1
$SHELL
