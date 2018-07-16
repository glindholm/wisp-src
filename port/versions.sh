#
#	$Id:$
#
#	Usage: version.sh {version}
#

if [ ! -d ${WISPSRC} ]
then
	echo 'Directory ${WISPSRC} not found'
	exit
fi

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

F=${WISPSRC}/port/Versions_${VER}.txt

rm -f ${F}

echo "# Versions for WISP ${VER} on `date`" >${F}

cd ${WISPSRC}
cvs status \
 | grep "Repository revision:" \
 | sed "s/Repository revision://" \
 | sed "s!/cvsroot/!!" \
 | sed "s!/Attic/!/!" \
 | sed "s/,v//" \
 | awk '{print $2 "\t" $1}' >>${F}

cat ${F}
echo " "
echo "Created ${F}"
ls -l ${F}

