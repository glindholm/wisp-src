#!/bin/sh
# this script sends a file to a remote ilpsrv process
# requires 2 args, host and filename 
# on the remote machine, there must be a user named
# ilp, who owns the /usr/spool/iprint dir and all
# files in there, and a ilpsrv daemon
#
# it's best to set up a printer using this script
# to automatically print on a remote machine when
# using that printer
# 
host=$1
name=$2
ilpdir=/usr/spool/iprint

# autodir is the directory ilpsrv watches to automatically
# spool files which land there.  make sure autodir specified
# in ilpremote and ilpsrv agrees
autodir=$ilpdir/auto
autospool=$ilpdir/autospool

if [ ! -d $autospool ]
then
mkdir $autospool
chmod 777 $autospool
fi

cat >$autospool/tmp$$
rcp $autospool/tmp$$ ilp@$host:$autodir/$name
