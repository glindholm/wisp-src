
echo "This routine removes any files with names longer than 14 characters."
echo "Do you wish to continue?"
read ans
if [ "$ans" != "y" ]
then
	echo "Aborting..."
	exit 1
fi
echo "Continue."

cwd=`pwd`
dname=`basename $cwd`
if [ "$dname" = "src" ]
then
	echo "Changing to Wisp root directory."
	cd ../../..
	echo "Now in directory:"
	pwd
fi

for fil in `find . -name "???????????????*" -print`
do
	echo "File <$fil> name too long. Rename?"
	read ans
	if [ "$ans" = "y" ]
	then
		echo "Enter new name:"
		read nam
		bas=`dirname $fil`
		if [ -f $bas/$nam ]
		then
			echo "$bas/$name exists"
			ls $bas
			echo "Enter new name:"
			read nam
		fi
		echo "Rename $fil to $bas/$nam"
		mv $fil $bas/$nam
	else
		echo "Delete it?"
		read ans
		if [ "$ans" = "y" ]
		then
			echo "Delete $fil"
			rm $fil
		else
			echo "Name is still too long."
		fi
	fi
done
