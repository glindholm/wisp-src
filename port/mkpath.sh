
#	mkpath.sh	recursive command to build a path to a subdirectory.
#
cmd=$0
path=$1
parent=`dirname $path`
if [ ! -d $parent ]
then
	$cmd $parent
fi
#
mkdir $path
#
