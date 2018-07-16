#!/usr/bin/perl
#
#uncomment following lines if your system's (SCO is one)
# shell doesn't support #!
#
#eval "exec /usr/bin/perl -S $0 $*"
# if $running_under_some_shell;
#
# author: Jock Cooper IDSI 805-295-1155
# Copyright IDSI 1990,1991
# 
# $Log: bldmfmf.perl,v $
# Revision 1.6  1995/04/25 09:55:26  gsl
# drcs state V3_3_15
#
# Revision 1.5  1995/04/17  11:48:20  gsl
# drcs state V3_3_14
#
# Revision 1.4  1995/01/27  22:47:40  gsl
# drcs load
#
# Revision 1.1  1991/04/18  23:02:14  jockc
# Initial revision
#
#
# ---old sccs change log follow--
#  
#  D 1.9 91/02/15 09:19:31 root 9 8	00003/00001/00197
#  added comment about SCO shell brain damage
#  
#  D 1.8 91/02/14 17:29:01 root 8 7	00001/00001/00197
#  change egrep to grep
#  
#  D 1.7 91/01/15 14:38:24 root 7 6	00002/00001/00196
#  allow for cbl progs that say copy xxx of yyy
#  
#  D 1.6 91/01/09 13:59:02 root 6 5	00001/00001/00196
#  
#  D 1.5 91/01/09 13:57:41 root 5 4	00001/00000/00196
#  added quiet mode to help
#  
#  D 1.4 91/01/09 13:56:37 root 4 3	00002/00001/00194
#  added quiet mode
#  
#  D 1.3 91/01/09 13:53:23 root 3 2	00004/00001/00191
#  added option to compress .cob files after unsuccessful compile
#  
#  D 1.2 91/01/09 10:18:13 root 2 1	00002/00002/00190
#  created
#  
#  D 1.1 91/01/09 10:17:00 root 1 0	00192/00000/00000
#  date and time created 91/01/09 10:17:00 by root
#    
# do not modify
#
	$ident="$Id:$";
	$vers="$Revision:$";
	$date="$Date:$";
	$headcnt=50;
	$dest 		= "../run";
	$ccblcmd 	= "cob";
	$ccblswitch 	= " ";
	$output 	= "makefile";
	$wispexecdef 	= "wisp";
	$wispexec 	= $wispexecdef;
	$wispswitch 	= " ";
	while ($_ = $ARGV[0], /^-/)
	{
		shift;
		last if /^--$/;
		if (/^-r(.*)/)
		{
			if ($1 eq "") { $dest = $ARGV[0]; shift; }
			else { $dest = $1; }
		}
		if (/^-ws(.*)/) 
		{
			if ($1 eq "") { $wispswitch = $ARGV[0];  shift; }
			else { $wispswitch = $1; }
		}
		if (/^-cs(.*)/) 
		{
			if ($1 eq "") { $ccblswitch = $ARGV[0];  shift; }
			else { $ccblswitch = $1; }
		}
		if (/^-cm(.*)/)  
		{
			if ($1 eq "") { $ccblcmd = $ARGV[0];  shift; }
			else { $ccblcmd = $1; }
		}
		if (/^-wp(.*)/)  
		{
			if ($1 eq "") { $wispexec = $ARGV[0];  shift; }
			else { $wispexec = $1; }
		}
		if (/^-i(.*)/)
		{
			if ($1 eq "") { $incdir = $ARGV[0]; shift; }
			else { $incdir = $1; }
		}
		/^-warn/ && ($warn = 1);
		/^-e/ && ($logerr = 1);
		/^-d/ && ($delcob = 1);
		/^-cc/ && ($compcob = 1);
		/^-q/ && ($quietmode = 1);
		if (/^-o(.*)/)
		{
			if ($1 eq "") { $output = $ARGV[0];  shift; }
			else { $output = $1; }
		}
		if ($_ eq "-h")
		{
			print "\nbldmf $vers $date: Build makefile for COBOL projects\n\n";
			print "usage: bldmf [args]\n";
			print "\nPossible args:\n\t-r <destdir>\t\tDestination directory for run files\n";
			print "\t-ws <wisp switches>\tAdditional switches for wisp command\n";
			print "\t-cs <ccbl switches>\tAdditional switches for cobol compile command\n";
			print "\t-cm <ccbl name>\t\tCommand to use to compile .cob files\n";
			print "\t-i <include dir>\tDirectory containing copy books\n";
			print "\t-warn\t\t\tWarn about missing copy books\n";
			print "\t-d\t\t\tDelete .cob files after successful compilation\n";
			print "\t-cc\t\t\tCompress .cob files after compilation\n";
			print "\t-e\t\t\tLog errors to error.log during make\n";
			print "\t-o\t\t\tOutput filename (default: makefile)\n";
			print "\t-q\t\t\tQuiet mode\n";
			print "\t-wp <wisp path>\t\tPath of wisp translator (default: $wispexecdef)\n";
			print "\t-h\t\t\tThis help message\n";
			exit;
		}
	}

	print "\nbldmf v$vers $date- 'bldmf -h' for help\n\n";
	print "building program list...\n";
	open(FOO,"ls|");
	while (<FOO>)
	{
		$cur=$_;
		next if (! /.wcb$/);
		open(BAR,"head -$headcnt $_|");
		while (<BAR>)
		{
			if (/^[ 0-9]*IDENTIFICATION +DIVISION/) 
			{
				push(@wcbs,$cur); 
				if (! $quietmode) { print "$cur"; }
			}
		}
	}
	close(FOO);
	rename($output,$output . "orig");	# preserve old makefile
	open(MK,">$output");			# open new makefile for output
	print MK "# include mf.rules\n\n";
	print MK "RUNDIR    = $dest\n\n";
	print MK "WISP      = $wispexec\n";
	print MK "WISPFLAGS = -z $wispswitch\n";
	print MK "COBOL     = $ccblcmd\n";
	print MK "COBFLAGS  = -i -C warning=2 $ccblswitch\n";
	print MK "LANG      = -VMF\n";
	print MK "ext       =.int\n";
	print MK "MV        = mv\n";
	print MK "\n";
	print MK "all:";			# top level object is all
	while ($#wcbs >= $[) {			# cycle thru
		$file = pop(@wcbs);		#
		chop($file);
		$save = $file;
		$file =~ s/.wcb$//;		# chop off .wcb
		$file =~ tr/a-z/A-Z/; 		# convert to upper
		$file = $file . "\$(ext)";
		print MK "\t\$(RUNDIR)/$file";		# build a complete object ../run/xxxx
		$runnames{$save} = $file;
		if ($#wcbs >= $[) { print MK "\\\n"; }# put a \ unless it's the last
		else { print MK "\n"; }		#
	}
	print MK "\n";
	print "building dependency table...\n";
	@wcbs = ();
	open(FOO,"ls|");
	while (<FOO>)
	{
		$cur=$_;
		next if (! /.wcb$/);
		open(BAR,"head -$headcnt $_|");
		while (<BAR>)
		{
			if (/^[ 0-9]*IDENTIFICATION +DIVISION/) 
			{
				push(@wcbs,$cur); 
			}
		}
	}
	close(FOO);
	@deps=(); @line=();
	while ($#wcbs >= $[) {
		$tmp = pop(@wcbs);
		chop($tmp);
		$filecob = $filewcb = $tmp;
		$filecob =~ s/.wcb/.cob/;
		$fileerr = "error.log";
		open(DEP,"grep '^......[^*] *COPY ' $filewcb|");
		while (<DEP>)
		{
			($FILE) = /COPY +([A-Z0-9-]+)/;
			if (/COPY.*IN /) { ($LIB)  = /COPY.*IN +([A-Z0-9-]+)/; }
			else { ($LIB)  = /COPY.*OF +([A-Z0-9-]+)/; }
			$FILE =~ tr/A-Z/a-z/;
			$LIB =~ tr/A-Z/a-z/;
			if ($LIB) { $dep = "../$LIB/$FILE"; }
			else 
			{ 
				if ($incdir) { $dep = $incdir . "/$FILE"; }
				else { $dep = "./$FILE"; }
			}
			if (-f $dep . '.wcb') 
				{ push(@deps,$dep); }
			else 	{ print "warning: $dep.wcb not found [$filewcb]\n" if ($warn); }
		}

		$lowfile = $runnames{$filewcb};
		$lowfile =~ tr/A-Z/a-z/; 		# convert to lower
		print MK "\$(RUNDIR)/$runnames{$filewcb}: $filewcb";
		if ($#deps >= $[) { print MK "\\\n"; } else { print MK "\n"; }
		while ($#deps >= $[) 
		{
			$thisdep = pop(@deps);
			push(@line,join('.',($thisdep,'wcb'))) if -f $thisdep . '.wcb';
			if ($#line > 1) 
			{
				print MK "  @line";
				if ($#deps >= $[) { print MK "\\\n"; } else { print MK "\n"; }
				@line = ();
			}
		}
		if ($#line >= $[) { print MK "  @line\n"; @line = (); }
		if ($logerr) 
		{
			$errout = ">>$fileerr 2>&1";
		}
		print MK "\techo '****** Processing $filewcb' $errout \n";
		print MK "\t\$(WISP) \$(WISPFLAGS) \$(LANG) $filewcb $errout \n";
		print MK "\t\$(COBOL) \$(COBFLAGS) $filecob "; 
		if ($delcob) { print MK "&& rm -f $filecob"; }
		print MK " $errout \n";
		print MK "\t\$(MV) $lowfile \$(RUNDIR)/$runnames{$filewcb} $errout \n";

		if ($compcob) { print MK "\tif test -f $filecob; then compress $filecob; fi\n"; }
		print MK "\n";

		@deps=(); @line=();
	}
	print MK "\ncleanup:\n\trm -f *.errs\n\n";
 
