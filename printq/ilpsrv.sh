#!/bin/sh
#
# $Id:$
#
# this script watches the /usr/spool/iprint/auto directory,
# and prints whatever appears there (put by ilpremote)
#
# default class and form stuff below can be changed.
# you might want a class and form that do nothing to
# the file (no header, ffs, init string, or filters)
# so that the second idaemon to process the file 
# doesn't muck up what the first guy already did

# as of ilpremote revision 1.10, (Print Queue V2.21) this script is
# obsolete, so just exit if someone tries to run it.

exit 0
#
#class=X
#form=plain
#ilpdir=/usr/spool/iprint
#
# autodir is the directory ilpsrv watches to automatically
# spool files which land there.  make sure autodir specified
# in ilpremote and ilpsrv agrees
#autodir=$ilpdir/auto
# autospool is where files are moved to after they are spooled,
# because they must be moved out of autodir
#autospool=$ilpdir/autospool


#while true
#do
#	if [ ! -d $autodir ]
#	then
#	mkdir $autodir
#	chmod 777 $autodir
#	fi
#	if [ ! -d $autospool ]
#	then
#	mkdir $autospool
#	chmod 777 $autospool
#	fi
#	count=`/bin/ls $autodir|wc -l`
#	if [ $count -gt 0 ]
#	then
#		find $autodir -type f -print|
#			while read file
#			do
#				newfile=$autospool/`basename $file`
#				mv $file $newfile
#				ilp -C$class -f$form -Mdel $newfile >/dev/null 2>&1
#			done
#	fi
#	sleep 5
#done
#			
