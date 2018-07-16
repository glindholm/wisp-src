#!/bin/sh
#################################################################
#
#   Copyright (c) 1992 International Digital Scientific Inc.	
#	              All rights reserved.			
#									
#################################################################
#
#  File:            ulpremote
# 
#  Function:        Sample program for use with the UniQue Print Queue
#                   printer type=program
#
#  RCS ID:
#     $Id:$
#
#  Description:
#      This script receives a print file (from udaemon) as
#      standard input, and sends it to a remote system to be
#      printed by the udaemon there. 
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
#                     ulp to print it
#                     
#  Notes:
#      *** USAGE ***
#      
#      $ ulpremote myhost myfile myform prtname <myfile
# 	-or-
#      $ cat myfile | ulpremote myhost myfile myform prtname
#      
#	(-or- see example below for prtdef setup)
#      
#      ulpremote requires 4 args: 
#                         host name (of remote machine)
#			  filename  (of local print file)
#                         form name
#                         printer name
#      
#      Although ulpremote requires the filename as an argument,
#      you must supply the file as standard input to ulpremote.
#
#      TCP/IP considerations:
#      
#      Additionally, there must be a user named "ulp", who owns
#      the /usr/spool/uprint dir and all files in there.  This
#      is because ulpremote and ilpsrv use the TCP/IP tools rcp
#      and rsh, which require a remote user name which has
#      access to the target directories.  In the $HOME directory
#      of user ulp on the remote machine, the file .rhosts must
#      be created and must contain the names of all machines
#      which will access that remote machine.  Each line in the
#      .rhosts file should look like this:
#
#      .rhosts:
#           machinex root
#           machiney root
#
#      In this example, udaemons on machinex and machiney could
#      use ulpremote to print to this machine.
#      
#      You must also insure that the program "ulp" is available on the $PATH
#      of the remote ulp user.
#
#      Usage in prtdef:
#
#      This script can be used to automatically print on a
#      remote machine when using a specially configured local
#      printer.  In the prtdef file, the entry for such a
#      printer would look like this (assumes remote machine's
#      name is "myhost"):
#      
#      remote:
#	   lp="/usr/bin/ulpremote myhost $filename $form lp0"
#	   type=program
#	   class=Z
#	   form=whatever
#	 
#      The file will print on machine "myhost" on printer named
#      "lp0".
#      
#      If you have problems using printer type=program
#      (including the example above), look in the
#      /usr/spool/uprint/pstatus directory for a file name
#      <printername> (in this example, the filename
#      would be "remote").  Udaemon captures error
#      output from the printer program into that file.
#
#  History:             01/14/93    JEC added the new arguments, and
#                                   revised to no longer use 
#                                   ilpsrv 
#                       06/12/92    JEC  wrote this header
#				    added Lock mechanism to 
#				    prevent premature printing
#                       09/01/93    JEC modified to work with UniQue
#                       11/10/93    expand the usage and help information
#                                   in the header of this file
#
#
host=$1
name=$2
form=$3
dest=$4
ulpdir=/usr/spool/uprint

# workdir is a local dir used to hold temp files for ulpremote on
# both the local and remote systems. 
workdir=$ulpdir/remote

# create just in case (udaemon should do this for us)
if [ ! -d $workdir ]
then
mkdir $workdir
chmod 777 $workdir
fi

cat >$workdir/tmp$$

if rcp $workdir/tmp$$ ulp@$host:$workdir/`basename $name`.$$
then : ; else exit $? ; fi

if rsh $host -l ulp "ulp -Mdel -d$dest -f$form $workdir/`basename $name`.$$"
then : ; else exit $? ; fi

rm $workdir/tmp$$

exit 0

