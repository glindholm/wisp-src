#!/bin/sh
#################################################################
#
#   Copyright (c) 1992 International Digital Scientific Inc.	
#	              All rights reserved.			
#									
#################################################################
#
#  File:            ilpremote
# 
#  Function:        Sample program for use with IDSI Print Queue
#                   printer type=program
#
#  RCS ID:
#     $Id:$
#
#  Description:
#      This script receives a print file (from idaemon) as
#      standard input, and sends it to a remote system to be
#      printed by the idaemon there.  It is no longer necessary
#      to use the ilpsrv script as in versions 1.8 and below
#      of ilpremote.
#
#      Note that to use 1.10 or higher of ilpremote, you must have
#      2.18 or later of the Print Queue.  This is because the
#      this version of ilpremote requires the form name and 
#      remote printer destination name, which only 2.18 or 
#      later idaemons can provide.  
#      
#  Input:  <stdin>    contents of file to print
#          Argument1  name of remote machine
#          Argument2  name of file to print
#          Argument3  name of form to use (form must exist on
#                     the local as well as remote system)
#          Argument4  name of the printer to use on the 
#                     remote system
#
#  Output:            sends file to remote system, and uses
#                     ilp to print it
#                     
#  Notes:
#      *** USAGE ***
#      
#      $ ilpremote myhost myfile myform prtname <myfile
# 	-or-
#      $ cat myfile | ilpremote myhost myfile myform prtname
#      
#	(-or- see example below for prtdef setup)
#      
#      ilpremote requires 4 args: 
#                         host name (of remote machine)
#			  filename  (of local print file)
#                         form name
#                         printer name
#      
#      Although ilpremote requires the filename as an argument,
#      you must supply the file as standard input to ilpremote.
#      
#      Additionally, there must be a user named "ilp", who owns
#      the /usr/spool/iprint dir and all files in there.  This
#      is because ilpremote and ilpsrv use the TCP/IP tools rcp
#      and rsh, which require a remote user name which has
#      access to the target directories.
#      
#      This script can be used to automatically print on a
#      remote machine when using a specially configured local
#      printer.  In the prtdef file, the entry for such a
#      printer would look like this (assumes remote machine's
#      name is "myhost"):
#      
#      remote:
#	   lp="/usr/bin/ilpremote myhost $filename $form lp0"
#	   type=program
#	   class=Z
#	   form=whatever
#	 
#      The file will print on machine "myhost" on printer named
#      "lp0".
#      
#      If you have problems using printer type=program
#      (including the example above), look in the
#      /usr/spool/iprint/pstatus directory for a file name
#      <printername> (in this example, the filename
#      would be "remote").  Idaemon captures error
#      output from the printer program into that file.
#
#  History:             01/14/93    added the new arguments, and
#                                   revised to no longer use 
#                                   ilpsrv 
#                       06/12/92    JEC  wrote this header
#				    added Lock mechanism to 
#				    prevent premature printing
#
#
host=$1
name=$2
form=$3
dest=$4
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

if rcp $autospool/tmp$$ ilp@$host:$autospool/`basename $name`
then : ; else exit $? ; fi

if rsh $host -l ilp "ilp -d$dest -f$form $autospool/`basename $name`"
then : ; else exit $? ; fi

rm $autospool/tmp$$

exit 0

