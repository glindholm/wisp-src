#
#	addstd.sh
#
#	Add standard RCS id, copyrights, and log lines.
#
TFILE=temp$$

for i in *.c
do

	grep -q 'Id: ' $i
	if [ 1 = $? ]
	then
		echo Adding standard id to $i
		echo 'static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";' > $TFILE
		echo 'static char rcsid[]="$Id:$";'>> $TFILE
		cat $i >> $TFILE
		mv $TFILE $i
	else
		echo Id OK $i
	fi
done

for i in *.h
do

	grep -q 'Id: ' $i
	if [ 1 = $? ]
	then
		echo Adding standard id to $i
		echo '/* Copyright (c) 1988-1996 DevTech Migrations, All rights reserved. */' > $TFILE
		echo '/* $Id:$ */'>> $TFILE
		cat $i >> $TFILE
		mv $TFILE $i
	else
		echo Id OK $i
	fi
done

for i in *.c *.h
do

	grep -q 'Log: ' $i
	if [ 1 = $? ]
	then
		echo Adding standard Log to $i
		cat $i 					 > $TFILE
		echo '/*' 				>> $TFILE
		echo '**	History:' 		>> $TFILE
		echo '**	$''Log: addstd.sh,v $'	>> $TFILE
		echo '**'				>> $TFILE
		echo '**'				>> $TFILE
		echo '*/'				>> $TFILE
		mv $TFILE $i
	else
		echo Log OK $i
	fi
done

rm -f $TFILE
