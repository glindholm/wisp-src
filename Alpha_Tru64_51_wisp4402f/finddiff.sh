# finddiff.sh

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
