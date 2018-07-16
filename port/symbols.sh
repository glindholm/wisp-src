#$Id:$
#
#	symbols.sh *.a
#

if [ "$*" = "" ]
then
	echo Show all the defined symbols in a .a library...
	echo 'USAGE: symbols.sh {*.a}'
	exit 1
fi
nm -g -P $* | sed '/ U /d' | sed '/:$/d' | cut -d' ' -f1 | sort -u
