# finddiff.sh
#
# Run this from the original source dir against the modified
# source dir to get a list of which files were modified.

MODDIR=../src

for f in `find . -type f -print`
do
	cmp -s $f $MODDIR/$f
	RC=$?
	if [ $RC != 0 ]
	then
		echo $f
	fi
done
