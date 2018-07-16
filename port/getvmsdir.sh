#####! /usr/local/bin/bash
#
# getvmsdir.sh
#	Based on sftp, will copy a SCALOS directory to unix.
#
# sftp -- "surgical" FTP
#
# Michael S Zraly
# mzraly@cs.umb.edu
# 26 July 1992
#
# Based loosely on getfile, written by John Granrose
# for his anonymous FTP archive at pilot.njin.net.

mac=scalos
fil=/dev/null
use=true

dbg=false
ver=false
bin=false
hash=false
dir=

usage='[-dvbh] dir'

prog=`basename $0`

USAGE="usage: $prog $usage"

#
#	Parse options
#

while getopts dvbh c
do
	case $c in
	d)	dbg=true
		;;
	v)	ver=true
		;;
	b)	bin=true
		;;
	h)	hash=true
		;;
	*)	echo option -$c 1>&2
		echo $USAGE 1>&2
		exit 1
		;;
	esac
done

shift `expr $OPTIND - 1`


min=1

if test "$#" -lt "$min"
then
	echo too few args 1>&2
	echo $USAGE 1>&2
	exit 1
else
	dir=$1
	shift
fi

#
#	Now check for debugging option
#

if test $dbg = "true"
then
	echo "dbg  = \"$dbg\""
	echo "use  = \"$use\""
	echo "ver  = \"$ver\""
	echo "bin  = \"$bin\""
	echo "hash = \"$hash\""
	echo "mac  = \"$mac\""
	echo "dir  = \"$dir\""
	exit 0
fi

#
#	Set arguments to FTP command
#

if test "$use" = "false"
then
	optn=-n
fi

if test "$ver" = "true"
then
	optv=-v
fi

echo
echo GETTING $dir 

#
#	Now for the actual FTP'ing
#

(
	#
	#	Translate VMS style names
	#
	echo 'case'
	echo 'nmap $1.$2;$3 $1.$2'
	#
	#	If not using .netrc file, assume anonymous login
	#
	if test "$use" = "false"
	then
		echo user  NAME PASS
	fi
	#
	#	cd to base directory
	#
	if test -n "$dir"
	then
		echo cd $dir
	fi
	#
	#	take care of binary, hash commands if needed
	#
	if test "$bin" = "true"
	then
		echo binary
	fi
	if test "$hash" = "true"
	then
		echo hash
	fi
	#
	#	execute ftp command
	#
	echo 'mget *.*;'
	#
	#	all done
	#
	echo bye
) | ftp -i $optn $optv $mac

#
#	Exit with status of FTP command
#
exit $?
