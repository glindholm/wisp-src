#!/bin/bash
echo 'Platform Info:'
echo '===================='

echo
echo 'uname -s -r -v:'
echo '--------------------'
uname -s -r -v

echo
echo '/etc/*release files:'
echo '===================='
echo
for f in /etc/*release
do
echo ${f}:
echo '--------------------'
cat ${f}
echo
done

result=$(which oslevel 2>/dev/null)
if [ "$?" == "0" ]; then
echo 'oslevel -s:'
echo '--------------------'
oslevel -s
echo
fi

result=$(which lsb_release 2>/dev/null)
if [ "$?" == "0" ]; then
echo 'lsb_release -a:'
echo '-------------------'
lsb_release -a 2>&1
echo
fi

