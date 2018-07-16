
#	dgux-l		Special build for DG/UX library "L" routines.
#
#			The "make" utility does not make files starting with
#			the letter "l" (ell); so they need this routine to
#			add them to a library.
for lc in l*.c
do
	lo=`basename $lc .c`.o
	echo "Making $lo"
	cc -DDGUX -c $lc
done
echo "Add to archives."
for lar in *.a
do
	echo "Add to $lar"
	ar rvs $lar l*.o
	ar ts  $lar
	cp $lar /usr/lib
done
echo "Complete."
#
