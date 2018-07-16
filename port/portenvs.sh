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
if [ "$COBDIR" = "" ]
then
	COBDIR=/usr/lib/cobol
	export COBDIR
fi

echo WISPSRC=${WISPSRC}
echo WISPDIR=${WISPDIR}
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
echo SHELL=${SHELL}
echo
echo Switching to new shell for WISP PORT
echo
PS1="WISP PORT `hostname` $ "
export PS1
$SHELL
