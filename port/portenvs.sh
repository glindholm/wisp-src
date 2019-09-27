#
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
WISPDIR=`pwd`
export WISPDIR
WISPSRC=`pwd`
export WISPSRC

cd ${WISPSRC}/port
if [ ${SAVEPWD} != ${WISPSRC}/port ]
then
	echo ERROR must be run from wisp/src/port directory
	exit 1
fi

if [ "$ACUDIR" = "" ]
then
	ACUDIR=/usr/local/acucobol
	export ACUDIR
fi
if [ "${A_TERMCAP}" = "" ]
then
	 export A_TERMCAP=${ACUDIR}/etc/a_termcap
fi
if [ "$COBDIR" = "" ]
then
	COBDIR=/usr/lib/cobol
	export COBDIR
fi
if [ "${WISPTMPDIR}" = "" ]
then
	export WISPTMPDIR=/var/tmp
fi
if [ "${WISPGID}" = "" ]
then
	export WISPGID=$$
fi


echo WISPSRC=${WISPSRC}
echo WISPDIR=${WISPDIR}
echo WISPTMPDIR=${WISPTMPDIR}
echo ACUDIR=${ACUDIR}
if [ ! -d ${ACUDIR} ]
then
	echo '**** ACUDIR not found ****'
fi
echo COBDIR=${COBDIR}
if [ ! -d ${COBDIR} ]
then
	echo '**** COBDIR not found ****'
fi
if [ ! -d ${WISPTMPDIR} ]
then
	echo '**** WISPTMPDIR not found ****'
fi
echo SHELL=${SHELL}
echo
echo Switching to new shell for WISP PORT
echo
PS1='WISP PORT '`uname`' [${PWD}] \$ '
export PS1
$SHELL
