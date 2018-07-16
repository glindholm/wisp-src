#!/bin/sh
#
#
#	File:		configacu.sh
#
#	Function:	This script builds the testacu ${WISPCONFIG}.
#
#	Desciption:	This routine is run after WISP has been built
#			on the machine.  It builds the ${WISPCONFIG}
#			directory for testing ACUCOBOL.
#			
#				${WISPSRC}/testacu/config
#
#	Input:		${WISPSRC}/...	The WISP source directory.
#
#	Output:		${WISPSRC}/testacu/config/...
#					The WISP config directory.
#
#	History:	02/18/93	Written by GSL
#

SCRIPT=configacu.sh

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
	

TESTDIR=${WISPSRC}/testacu
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
echo "VOLIN  ${WISPSRC}/testacu/volin"	>> ${WISPCONFIG}/LGMAP
echo "VOLOUT ${WISPSRC}/testacu/volout"	>> ${WISPCONFIG}/LGMAP
echo "VOLRUN ${WISPSRC}/testacu/volrun"	>> ${WISPCONFIG}/LGMAP
echo "VOLSPL ${WISPSRC}/testacu/volspl"	>> ${WISPCONFIG}/LGMAP
echo "VOLWRK ${WISPSRC}/testacu/volwrk"	>> ${WISPCONFIG}/LGMAP
echo "TEST   ${WISPSRC}/testacu"	>> ${WISPCONFIG}/LGMAP


# Create wrunconfig
echo "cobol=ACU"			>  ${WISPCONFIG}/wrunconfig
echo "options=-b +e acuerr.log"		>> ${WISPCONFIG}/wrunconfig
echo "runcbl=${WISPDIR}/kcsiacu/wruncblk"	>> ${WISPCONFIG}/wrunconfig

echo Loading ${WISPCONFIG}/videocap
cp ${WISPDIR}/config/videocap/* ${WISPCONFIG}/videocap

${WISPDIR}/bin/wsysconf


echo FILE-STATUS-CODE 74 		                            > ${WISPCONFIG}/ACUCONFIG
echo CODE-PREFIX ${WISPSRC}/testacu ${WISPDIR}/acu ${WISPDIR}/kcsiacu . >> ${WISPCONFIG}/ACUCONFIG
echo ALTVARV4-VERSION 4			                           >> ${WISPCONFIG}/ACUCONFIG
echo ALTVARV3-VERSION 3			                           >> ${WISPCONFIG}/ACUCONFIG
echo ALTVARV2-VERSION 2			                           >> ${WISPCONFIG}/ACUCONFIG
echo KEYSTROKE EXCEPTION=33 5		         >> ${WISPCONFIG}/ACUCONFIG
echo KEYSTROKE EXCEPTION=34 6		         >> ${WISPCONFIG}/ACUCONFIG
echo KEYSTROKE EXCEPTION=34 27		         >> ${WISPCONFIG}/ACUCONFIG
echo SCREEN PROMPT=* PROMPT-ALL=YES	         >> ${WISPCONFIG}/ACUCONFIG

# Copy license file
echo Copy license file
HOSTNAME=`hostname`
HOSTLICENSE=${TESTDIR}/wisp.license.${HOSTNAME}
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
