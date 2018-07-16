
#	cutvcap.sh	Remove ".vcap" extension from all videocap files.
#			Also, clean up video cap directory.
#
for old in *.vcap
do
	new=`basename $old .vcap`
	echo "copy $old to $new"
	cp $old $new
done
#
if [ ! -d ../old ]
then
	echo "Making directory ../old"
	mkdir ../old
elif [ -f ../old ]
then
	echo "../old is a regular file, deleting and making it as a directory."
	rm	../old
	mkdir	../old
fi
#
echo "Moving .vcap and .sh files to ../old"
mv *.vcap *.sh ../old
#
