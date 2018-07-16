
#	cacbl		Compile ACUCOBOL source to P-Code.
#
if [ $# -ne 1 ]
then
	echo 'Usage: cacbl name    Where "name" is a cobol source file.'
	echo '                     A .cob file extension is infered.'
	exit 1
fi
#
csrc=`basename $1 .cob`
pnam=`echo $csrc | tr [a-z] [A-Z]`
ccbl -da4 -o $pnam $csrc.cob
#
