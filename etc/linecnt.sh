#!/bin/sh
########################################################################
#
#               WISP - Wang Interchange Source Pre-processor
#             Copyright (c) 1988,1989,1990,1991,1992,1993,1994
#        An unpublished work of International Digital Scientific Inc.
#                           All rights reserved.
#
########################################################################
#
#	linecnt		Do a line count on COBOL programs
#
#	Usage:		linecnt {files...}
#
#	The script counts the number of lines in a COBOL program 
#	excluding comment lines.
#
########################################################################

if [ 0 = $# ]
then
	echo 'Usage: linecnt {files...}'
	echo 
	echo '       The linecnt utility will count the number of non-comment lines'
	echo '       in COBOL programs and display the total.'
	echo 
	echo 'Example: To do a line count for a directory:'
	echo 
	echo '       $ linecnt *.wcb'
	exit 1
fi

total=0
for i in $*
do
	lc=`sed '/^......\*/d' $i|wc -l`
	echo "$lc\t$i"
	total=`expr $total + $lc`
done

echo
echo '**** TOTAL = ' $total ' ****'

exit 0
