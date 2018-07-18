#!/bin/sh
(echo "hi there\c" ; echo " ") >.echotmp
if egrep c .echotmp >/dev/null 2>&1 ; then
    n='-n'
    c=''
else
    n=''
    c='\c'
fi
rm .echotmp
echo $n "TERM [$TERM] : $c"
read X
if [ "$X" ]
then
TERM=$X
WISPTERM=$X
fi
export TERM
echo TERM=$TERM

echo $n "WISPTERM [${WISPTERM}] : $c"
read X
if [ "$X" ]
then
WISPTERM=$X
fi
export WISPTERM
echo WISPTERM=${WISPTERM}

X=`echo ${WISPTERM}| grep 'vt[01-9]*' `
if [ "$X" ]
then
echo 'stty erase "^?"' 
stty erase "^?"
else
echo 'stty erase "^h"' 
stty erase "^h"
fi

