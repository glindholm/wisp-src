echo
echo This script will move all the old source to dir old.
echo
echo	'Do you wish to continue [y/n] ? '
read ANS
if [ "y" != "${ANS}" ]
then
echo	exiting
exit
else
echo	continuing...
fi
echo
echo 	'Enter parent directory (e.g. /usr2) ? '
read DIR

cd $DIR/video/lib/src
echo
echo 	pwd = `pwd`
echo	'move to old [y/n] ? '
read ANS
if [ "y" = "${ANS}" ]
then
	mkdir old
	mv *.c *.h *.umf *.sh old
	ls -x
	echo 'Do a rm * [y/n] ? '
	read ANS
	if [ "y" = "${ANS}" ]
	then
	rm *
	fi
fi

cd $DIR/video/test/src
echo
echo 	pwd = `pwd`
echo	'move to old [y/n] ? '
read ANS
if [ "y" = "${ANS}" ]
then
	mkdir old
	mv *.c *.h *.umf *.sh old
	ls -x
	echo 'Do a rm * [y/n] ? '
	read ANS
	if [ "y" = "${ANS}" ]
	then
	rm *
	fi
fi

cd $DIR/video/cap/src
echo
echo 	pwd = `pwd`
echo	'move to old [y/n] ? '
read ANS
if [ "y" = "${ANS}" ]
then
	mkdir old
	mv * old
	ls -x
	echo 'Do a rm * [y/n] ? '
	read ANS
	if [ "y" = "${ANS}" ]
	then
	rm *
	fi
fi

cd $DIR/wisp/acu/src
echo
echo 	pwd = `pwd`
echo	'move to old [y/n] ? '
read ANS
if [ "y" = "${ANS}" ]
then
	mkdir old
	mv * old
	ls -x
	echo 'Do a rm * [y/n] ? '
	read ANS
	if [ "y" = "${ANS}" ]
	then
	rm *
	fi
fi

cd $DIR/wisp/mf/src
echo
echo 	pwd = `pwd`
echo	'move to old [y/n] ? '
read ANS
if [ "y" = "${ANS}" ]
then
	mkdir old
	mv * old
	ls -x
	echo 'Do a rm * [y/n] ? '
	read ANS
	if [ "y" = "${ANS}" ]
	then
	rm *
	fi
fi

cd $DIR/wisp/ede/src
echo
echo 	pwd = `pwd`
echo	'move to old [y/n] ? '
read ANS
if [ "y" = "${ANS}" ]
then
	mkdir old
	mv *.c *.h *.cob *.wcb *.umf *.sh old
	ls -x
	echo 'Do a rm * [y/n] ? '
	read ANS
	if [ "y" = "${ANS}" ]
	then
	rm *
	fi
fi

cd $DIR/wisp/etc/src
echo
echo 	pwd = `pwd`
echo	'move to old [y/n] ? '
read ANS
if [ "y" = "${ANS}" ]
then
	mkdir old
	mv * old
	ls -x
	echo 'Do a rm * [y/n] ? '
	read ANS
	if [ "y" = "${ANS}" ]
	then
	rm *
	fi
fi

cd $DIR/wisp/lib/src
echo
echo 	pwd = `pwd`
echo	'move to old [y/n] ? '
read ANS
if [ "y" = "${ANS}" ]
then
	mkdir old
	mv *.c *.h *.umf *.sh old
	ls -x
	echo 'Do a rm * [y/n] ? '
	read ANS
	if [ "y" = "${ANS}" ]
	then
	rm *
	fi
fi

cd $DIR/wisp/printq/src
echo
echo 	pwd = `pwd`
echo	'move to old [y/n] ? '
read ANS
if [ "y" = "${ANS}" ]
then
	mkdir old
	mv *.c *.h *.l *.y *.umf *.sh old
	ls -x
	echo 'Do a rm * [y/n] ? '
	read ANS
	if [ "y" = "${ANS}" ]
	then
	rm *
	fi
fi

cd $DIR/wisp/proctran/src
echo
echo 	pwd = `pwd`
echo	'move to old [y/n] ? '
read ANS
if [ "y" = "${ANS}" ]
then
	mkdir old
	mv *.c *.h *.umf *.sh old
	ls -x
	echo 'Do a rm * [y/n] ? '
	read ANS
	if [ "y" = "${ANS}" ]
	then
	rm *
	fi
fi

cd $DIR/wisp/test/src
echo
echo 	pwd = `pwd`
echo	'move to old [y/n] ? '
read ANS
if [ "y" = "${ANS}" ]
then
	mkdir old
	mv *.c *.h *.umf *.wcb *.sh *.opt *.key old
	ls -x
	echo 'Do a rm * [y/n] ? '
	read ANS
	if [ "y" = "${ANS}" ]
	then
	rm *
	fi
fi

cd $DIR/wisp/tran/src
echo
echo 	pwd = `pwd`
echo	'move to old [y/n] ? '
read ANS
if [ "y" = "${ANS}" ]
then
	mkdir old
	mv *.c *.h *.umf *.sh old
	ls -x
	echo 'Do a rm * [y/n] ? '
	read ANS
	if [ "y" = "${ANS}" ]
	then
	rm *
	fi
fi

cd $DIR/wisp/utils/src
echo
echo 	pwd = `pwd`
echo	'move to old [y/n] ? '
read ANS
if [ "y" = "${ANS}" ]
then
	mkdir old
	mv *.c *.h *.umf *.sh *.txt *.wcb old
	ls -x
	echo 'Do a rm * [y/n] ? '
	read ANS
	if [ "y" = "${ANS}" ]
	then
	rm *
	fi
fi

cd $DIR/wisp/msdos/src
echo
echo 	pwd = `pwd`
echo	'move to old [y/n] ? '
read ANS
if [ "y" = "${ANS}" ]
then
	mkdir old
	mv *.c *.h *.umf *.sh old
	ls -x
	echo 'Do a rm * [y/n] ? '
	read ANS
	if [ "y" = "${ANS}" ]
	then
	rm *
	fi
fi

cd $DIR/wisp/port/src
echo
echo 	pwd = `pwd`
echo	'move to old [y/n] ? '
read ANS
if [ "y" = "${ANS}" ]
then
	mkdir old
	mv *.c *.h *.umf *.sh old
	ls -x
	echo 'Do a rm * [y/n] ? '
	read ANS
	if [ "y" = "${ANS}" ]
	then
	rm *
	fi
fi

echo 
echo Done.
echo
exit
