#!/bin/sh
#
#	File:		installqa.sh
#
#	Function:	This script installs a QA version of wisp 
#			to $WISP/wisp.QA.
#
#	Desciption:	This routine "installs" the QA ship kit.
#			- remove old $WISP/wisp.QA
#			- create $WISP/wisp.QA
#			- copy $WISP/src/ship/wisp to $WISP/wisp.QA
#			- add EDE and the runtime systems
#
#	Warning:	The previous version of $WISP/wisp.QA will be
#			deleted.
#
#	History:	06/30/92	Written. GSL
#

if [ ! -d $WISP ]
then
	echo 'Directory $WISP not found'
	echo installqa.sh Aborting!
	exit
fi

echo
echo Available WISP shipping kits:
echo
ls -d $WISP/src/wisp_*.ship

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
SHIP=$WISP/src/${WISP_VER}.ship
SHIPWISPTARZ=$SHIP/${WISP_VER}.tar.Z
SHIPEDETARZ=$SHIP/${EDE_VER}.tar.Z
QABASE=${WISP}/QA
WISPDIR=${WISP}/QA/wisp

if [ ! -f ${SHIPWISPTARZ} ]
then
	echo "${SHIPWISPTARZ} does not exist, run bldshipkit.sh"
	echo installqa.sh Aborting!
	exit
fi

#
#	Cleanup old QA
#		$WISP/QA
#		$WISP/wisp.QA --> $WISP/QA/wisp_XXXX
#
if [ -d ${QABASE} ]
then
	echo
	echo 'Removing OLD $WISP/QA'
	rm -r -f ${QABASE}

	if [ -d ${QABASE} ]
	then
		echo Unable to remove ${QABASE}
		echo installqa.sh Aborting!
		exit
	fi
fi

#
#	Make a new $WISP/QA
#
echo
echo 'Creating $WISP/QA'
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
uncompress -c ${SHIPWISPTARZ} | tar -xvpf - 
ln -s ${WISP_VER} wisp

echo
echo "Installing ${SHIPEDETARZ}"
echo "        to  ${WISPDIR}/ede"
echo

cd ${WISPDIR}
uncompress -c ${SHIPEDETARZ} | tar -xvpf - 
ln -s ${EDE_VER} ede

echo
echo 'Adding CRID for Acucobol'
CRIDSHIP=$WISP/src/kcsi/crid_acu_*.ship
if [ -d $CRIDSHIP ]
then
	cd ${WISPDIR}
	uncompress -c $CRIDSHIP/crid_acu_*.tar.Z|tar -xvpf -
	ln -s crid_acu_* cridacu
else
	echo
	echo '**** CRID ACU NOT INSTALLED ****'
	echo
fi

echo
echo 'Adding CRID for Micro Focus'
CRIDSHIP=$WISP/src/kcsi/crid_mf_*.ship
if [ -d $CRIDSHIP ]
then
	cd ${WISPDIR}
	uncompress -c $CRIDSHIP/crid_mf_*.tar.Z|tar -xvpf -
	ln -s crid_mf_* cridmfx
else
	echo
	echo '**** CRID MF NOT INSTALLED ****'
	echo
fi

echo
echo 'Adding CREATE for Acucobol'
CREATESHIP=$WISP/src/kcsi/create_acu_*.ship
if [ -d $CREATESHIP ]
then
	cd ${WISPDIR}
	uncompress -c $CREATESHIP/create_acu_*.tar.Z|tar -xvpf -
	ln -s create_acu_* createacu
else
	echo
	echo '**** CREATE ACU NOT INSTALLED ****'
	echo
fi

echo
echo 'Adding CREATE for Micro Focus'
CREATESHIP=$WISP/src/kcsi/create_mf_*.ship
if [ -d $CREATESHIP ]
then
	cd ${WISPDIR}
	uncompress -c $CREATESHIP/create_mf_*.tar.Z|tar -xvpf -
	ln -s create_mf_* createmfx
else
	echo
	echo '**** CREATE MF NOT INSTALLED ****'
	echo
fi


echo
echo 'Building ACUCOBOL runtime systems'
echo 

cd ${WISPDIR}/acu
make ACUDIR=${ACUDIR} WISPDIR=${WISPDIR} -f wruncbl.umf
make ACUDIR=${ACUDIR} WISPDIR=${WISPDIR} EDEDIR=${WISPDIR}/ede -f wruncbl.umf ede


echo
echo 'Building Micro Focus runtime systems'
echo 
if [ -d $COBDIR ]
then
	cd ${WISPDIR}/mf
	make COBDIR=${COBDIR} WISPDIR=${WISPDIR} -f wrunmf.umf
	make COBDIR=${COBDIR} WISPDIR=${WISPDIR} EDEDIR=${WISPDIR}/ede -f wrunmf.umf ede
else
	echo
	echo '**** MF COBOL NOT AVAILBALE ****'
	echo
fi


if [ -f ${WISPDIR}/cridacu/wruncbl.umf ]
then
	echo
	echo 'Building the ACU+CRID runtime'
	echo 

	cd ${WISPDIR}/cridacu
	make ACUDIR=${ACUDIR} WISPDIR=${WISPDIR} CRIDDIR=${WISPDIR}/cridacu -f wruncbl.umf crid
else
	echo
	echo '**** CRID ACU is NOT INSTALLED ****'
	echo
fi

echo
echo '*** INSTALL QA DONE ***'

