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
COBSW=-F
COBPATH=:${QA}
PATH=${WISPDIR}/bin:${WISPDIR}/mf:${WISPDIR}/kcsimf:${QA}/volrun/onpath:${PATH}
export QA WISPCONFIG COBSW COBPATH PATH WISPDIR WISPSRC
echo
echo '**** SETTING UP FOR MICRO FOCUS ****'
echo "WISPDIR      = ${WISPDIR}"
echo "WISPSRC      = ${WISPSRC}"
echo "QA           = ${QA}"
echo "WISPCONFIG   = ${WISPCONFIG}"
echo "COBDIR       = ${COBDIR}"
echo "COBPATH      = ${COBPATH}"
echo "COBSW        = ${COBSW}"
echo "SHELL        = ${SHELL}"
echo "PATH         = ${PATH}"

COBVER=${COBDIR}/etc/cobver
if [ ! -f ${COBVER} ]
then
COBVER=${COBDIR}/cobver
fi
cat ${COBVER}
wisp|grep Version

# If needed create ${WISPCONFIG} 
if [ ! -d ${WISPCONFIG} ]
then
	configmf.sh
fi
wusage set PROGVOL=SOURCE
wusage set PROGLIB=TESTMF
wusage set RUNVOL=SOURCE
wusage set RUNLIB=TESTMF

echo
echo Switching to new shell for WISP QA
echo
# PS1="TESTMF `hostname` $ "
PS1="TESTMF `uname -n` $ "
export PS1
$SHELL
