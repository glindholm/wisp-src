#!/bin/sh
#	Id:		$Id:$
#
#	File:		installqa.sh
#
#	Function:	This script installs a QA version of wisp 
#			to ${WISPSRC}/QA.
#
#	Desciption:	This routine "installs" the QA ship kit.
#
#	Warning:	The previous version of ${WISPSRC}/QA will be
#			deleted.
#
if [ "${WISPSRC}" = "" ]
then
	WISPSRC=${WISP}/src
	export WISPSRC
	echo WISPSRC not set using ${WISPSRC}
fi

if [ ! -d ${WISPSRC} ]
then
	echo 'Directory ${WISPSRC} not found'
	echo installqa.sh Aborting!
	exit
fi

echo
echo Available WISP shipping kits:
echo
ls -d ${WISPSRC}/wisp_*.ship

echo
echo "Enter the WISP version number (e.g. 4400) ?"
read ANS
if [ "$ANS" = "" ]
then
	echo Version can not be blank.
	exit 1
fi
VER=$ANS
echo
echo Using Version=[${VER}]

#
#	Define some variables
#
WISP_VER=wisp_${VER}
EDE_VER=ede_${VER}
SHIP=${WISPSRC}/${WISP_VER}.ship
SHIPWISPTARZ=$SHIP/${WISP_VER}.tar.gz
SHIPEDETARZ=$SHIP/${EDE_VER}.tar.gz
QABASE=${WISPSRC}/QA
WISPDIR=${WISPSRC}/QA/wisp

if [ ! -f ${SHIPWISPTARZ} ]
then
	echo "${SHIPWISPTARZ} does not exist, run bldshipkit.sh"
	echo installqa.sh Aborting!
	exit
fi

#
#	Cleanup old QA
#		${WISPSRC}/QA
#
if [ -d ${QABASE} ]
then
	echo
	echo 'Removing OLD ${WISPSRC}/QA'
	rm -r -f ${QABASE}

	if [ -d ${QABASE} ]
	then
		echo Unable to remove ${QABASE}
		echo installqa.sh Aborting!
		exit
	fi
fi

#
#	Make a new ${WISPSRC}/QA
#
echo
echo 'Creating ${WISPSRC}/QA'
mkdir ${QABASE}

if [ ! -d ${QABASE} ]
then
	echo Unable to create ${QABASE}
	echo installqa.sh Aborting!
	exit
fi


echo
echo "Installing ${SHIPWISPTARZ}"
echo "        in ${WISPDIR}"
echo

cd ${QABASE}
tar -xzvpf ${SHIPWISPTARZ}
ln -s ${WISP_VER} wisp

echo
echo "Installing ${SHIPEDETARZ}"
echo "        to  ${WISPDIR}/ede"
echo

cd ${WISPDIR}
tar -xzvpf ${SHIPEDETARZ}
ln -s ${EDE_VER} ede

echo
echo 'Adding KCSI for Acucobol'
KCSISHIP=${WISPSRC}/kcsi/kcsi_acu_*.ship
if [ -d ${KCSISHIP} ]
then
	cd ${WISPDIR}
	utar -xzvpf $KCSISHIP/kcsi_acu_*.tar.gz
	ln -s kcsi_acu_* kcsiacu
else
	echo
	echo '**** KCSI ACU NOT INSTALLED ****'
	echo
fi

echo
echo 'Adding KCSI for Micro Focus'
KCSISHIP=${WISPSRC}/kcsi/kcsi_mf_*.ship
if [ -d ${KCSISHIP} ]
then
	cd ${WISPDIR}
	tar -xzvpf ${KCSISHIP}/kcsi_mf_*.tar.gz
	ln -s kcsi_mf_* kcsimf
else
	echo
	echo '**** KCSI MF NOT INSTALLED ****'
	echo
fi

echo
echo 'Building ACUCOBOL runtime systems'
echo 

cd ${WISPDIR}/acu
make ACUDIR=${ACUDIR} WISPDIR=${WISPDIR} -f wruncbl.umf
#make ACUDIR=${ACUDIR} WISPDIR=${WISPDIR} EDEDIR=${WISPDIR}/ede -f wruncbl.umf ede


echo
echo 'Building Micro Focus runtime systems'
echo 
if [ -d $COBDIR ]
then
	cd ${WISPDIR}/mf
	make COBDIR=${COBDIR} WISPDIR=${WISPDIR} -f wrunmf.umf
	make COBDIR=${COBDIR} WISPDIR=${WISPDIR} -f wrunmf.umf rtsx4400
	make COBDIR=${COBDIR} WISPDIR=${WISPDIR} EDEDIR=${WISPDIR}/ede -f wrunmf.umf ede
else
	echo
	echo '**** MF COBOL NOT AVAILBALE ****'
	echo
fi


if [ -f ${WISPDIR}/kcsiacu/wruncbl.umf ]
then
	echo
	echo 'Building the ACU+KCSI runtime'
	echo 

	cd ${WISPDIR}/kcsiacu
	make ACUDIR=${ACUDIR} WISPDIR=${WISPDIR} KCSIDIR=${WISPDIR}/kcsiacu -f wruncbl.umf kcsi
else
	echo
	echo '**** KCSI ACU is NOT INSTALLED ****'
	echo
fi

echo
echo '*** INSTALL QA DONE ***'

