
ctest=`echo "X \c"`
if [ "X \c" = "$ctest" ]
then
#		"NOT AT&T line"
LD=1
else
#		"AT&T line"
LD=0
fi

ntest=`echo -n "X"`
if [ "-n X" = "$ntest" ]
then
#		"NOT berk line"
LD=0
else
#		"berk line"
LD=1
fi

exit $LD

