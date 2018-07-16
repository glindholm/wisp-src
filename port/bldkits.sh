
#	bldkits		Build the "wisp.cpio.Z" and "ede.cpio.Z" shipping kits.
#
echo " "
echo "This routine changes the protection of the shipping kit"
echo "and builds the wisp and ede distribution files."
echo " "
echo "Normally, this is run as part of the PORTUNIX.LIS procedure."
echo " "
echon "Are you sure this is what you want to do right now? "
read ans
if [ ! "$ans" = "y" ]
then
	echo "bldkits: abort."
	exit 1
fi
#
echo "CD to shipping directory: $SHIP"
cd $SHIP
#
echo 'Change protection of files in wisp and ede allowing others "r" (read).'
find ede wisp -exec chmod o+r {} \;
#
echo 'Change protection of files in wisp/bin allowing others "x" (execute).'
find wisp/bin -exec chmod o+x {} \;
#
echo "Build wisp file list with find command"
find wisp -print > wisp.fl
#
echo "Build  ede file list with find command"
find  ede -print >  ede.fl
#
echo "Build wisp.cpio file archive"
cpio -ocv > wisp.cpio < wisp.fl
#
echo "Build  ede.cpio file archive"
cpio -ocv >  ede.cpio <  ede.fl
#
echo "Compress wisp.cpio"
compress wisp.cpio
#
echo "Compress  ede.cpio"
compress  ede.cpio
#
echo " "
ls -l *.Z
#
