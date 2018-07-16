
#	wver	display wisp version number from the $WL/wcommon.h file.
#
#	Usage: wver format
#
#	format
#	L = Long version in original case.
#	o = Long version in original case.
#	l = Long version converted to lower case.
#	s = Short version.
#
default=L	# If no format, use Original/long format.
#
if [ $# -gt 0 ]
then
	format=$1
else
	format=$default
fi

case $format in
#
	L | o)
#
#		Original case long version <V3.1a>:
	grep WISP_VERSION $WL/w*common.h | cut -f2 -d'"'
		;;
#
	l)
#
#		Lower case long version <v3.1a>:
	grep WISP_VERSION $WL/w*common.h | cut -f2 -d'"' | tr [A-Z] [a-z]
		;;
#
	s)
#
#		Short version <31a>:
	grep WISP_VERSION $WL/w*common.h | cut -f2 -d'"' | cut -c2,4-
		;;
#
	*)
#
#		Show usage:
	echo	"Usage: wver [lLos]"
		;;
#
esac
