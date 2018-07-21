#!/bin/sh
#
#	File:		configmf.sh
#
#	Function:	This script builds the testmf ${WISPCONFIG}.
#
#	Desciption:	This routine is run after WISP has been built
#			on the machine.  It builds the ${WISPCONFIG}
#			directory for testing Micro Focus COBOL.
#			
#				${WISPSRC}/testmf/config
#
#	Input:		${WISPSRC}/...	The WISP source directory.
#
#	Output:		${WISPSRC}/testmf/config/...
#					The WISP config directory.
#
#

SCRIPT=configmf.sh

if [ x${WISPSRC} = x ]
then
	echo
	echo Variable \${WISPSRC} is not set!
	echo $SCRIPT ABORTING!
	exit
fi

if [ x${WISPDIR} = x ]
then
	echo
	echo Variable \${WISPDIR} is not set!
	echo $SCRIPT ABORTING!
	exit
fi

if [ ! -d ${WISPDIR} ]
then
	echo
	echo Directory ${WISPDIR} does not exist!
	echo $SCRIPT ABORTING!
	exit
fi

TESTDIR=${WISPSRC}/testmf
if [ ! -d ${TESTDIR} ]
then
	echo
	echo Directory ${TESTDIR} does not exist!
	echo $SCRIPT ABORTING!
	exit
fi

#
#	Define some variables
#
WISPCONFIG=${TESTDIR}/config
export WISPCONFIG

#
#	Create all the ${WISPCONFIG} directories
#
if [ -d ${WISPCONFIG} ]
then
	echo
	echo Directory ${WISPCONFIG} already exists
	echo $SCRIPT ABORTING!
	exit
fi

echo Creating ${WISPCONFIG}
mkdir ${WISPCONFIG}
mkdir ${WISPCONFIG}/videocap
if [ ! -d ${WISPCONFIG} ]
then
	echo
	echo Unable to create ${WISPCONFIG}
	echo $SCRIPT ABORTING!
	exit
fi

echo Loading ${WISPCONFIG}
cp ${WISPDIR}/config/CHARMAP	${WISPCONFIG}
cp ${WISPDIR}/config/FORMS	${WISPCONFIG}
cp ${WISPDIR}/config/LPMAP	${WISPCONFIG}
cp ${WISPDIR}/config/PRMAP	${WISPCONFIG}
cp ${WISPDIR}/config/SCMAP	${WISPCONFIG}
cp ${WISPDIR}/config/W4WMAP	${WISPCONFIG}
cp ${WISPDIR}/config/wproc.msg	${WISPCONFIG}

cat ${WISPDIR}/config/OPTIONS |sed "s|#PQLP|PQLP|"> ${WISPCONFIG}/OPTIONS
echo "SOURCE ${WISPSRC}"			>  ${WISPCONFIG}/LGMAP
echo "VOLIN  ${WISPSRC}/testmf/volin"	>> ${WISPCONFIG}/LGMAP
echo "VOLOUT ${WISPSRC}/testmf/volout"	>> ${WISPCONFIG}/LGMAP
echo "VOLRUN ${WISPSRC}/testmf/volrun"	>> ${WISPCONFIG}/LGMAP
echo "VOLSPL ${WISPSRC}/testmf/volspl"	>> ${WISPCONFIG}/LGMAP
echo "VOLWRK ${WISPSRC}/testmf/volwrk"	>> ${WISPCONFIG}/LGMAP
echo "TEST   ${WISPSRC}/testmf"		>> ${WISPCONFIG}/LGMAP

# Create wrunconfig
echo "cobol=MF"				>  ${WISPCONFIG}/wrunconfig
echo "options="				>> ${WISPCONFIG}/wrunconfig
echo "runcbl=${WISPDIR}/mf/wrunmf"	>> ${WISPCONFIG}/wrunconfig

echo Loading ${WISPCONFIG}/videocap
cp ${WISPDIR}/config/videocap/* ${WISPCONFIG}/videocap

${WISPDIR}/bin/wsysconf

# Copy license file
echo Copy license file
#HOSTNAME=`hostname`
HOSTNAME=`uname -n`
HOSTLICENSE=${TESTDIR}/wisp.license.${HOSTNAME}.txt
if [ -f ${HOSTLICENSE} ]
then
	echo cp ${HOSTLICENSE} ${WISPCONFIG}/wisp.license
	cp ${HOSTLICENSE} ${WISPCONFIG}/wisp.license
else
	echo
	echo ${HOSTLICENSE} NOT FOUND
	echo
fi

echo
echo The '${WISPCONFIG}' ${WISPCONFIG} directory has been built.
echo
echo
