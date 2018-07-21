/*
******************************************************************************
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
**
**
** NOTICE:
** Confidential, unpublished property of NeoMedia Technologies, Inc.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of NeoMedia 
** Technologies, Inc. is strictly prohibited.
** 
** CVS
**
** $Author: gsl $
**
**
******************************************************************************
*/

/*
**	selectpg.c	selectpg [-l#] start:end [infile]
**				-l#		Number of lines per page, default is 66 lpp.
**				-?		Print help page.
**				start:end	Page to start and end page numbers to write.
**				infile		Optional infile overrides stdin.
**
**			This routine reads from stdin and writes to stdout starting at the specified page.
*/


#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
static int selectpages();
static void  printusage();

#define PROGNAME "selectpg"

int main(argc,argv)
int	argc;
char	*argv[];
{
	int	c;
	extern char *optarg;
	extern int  optind;
	extern int  opterr;

	char	*o_lpp;
	char	*o_start;
	char	*o_infile;

	int	lpp;
	int	start, end;
	FILE	*f_in;

	char	message[80];
	int	rc;

	opterr = 0;

	o_lpp = 0;
	o_start = 0;
	o_infile = 0;

	lpp = 66;
	start = 0;
	end = 0;

	while((c = getopt(argc,argv,"?l:")) != -1)
	{
		switch(c)
		{
		case 'l':
			o_lpp=optarg;
			break;
		case '?':
			printusage(0);
			break;
		default:
			sprintf(message,"Illegal option -- %c",c);
			printusage(message);
		}
	}

	if ( optind < argc )
	{
		o_start = argv[optind];
		optind++;
	}
	else
	{
		printusage("Starting pagenum not given.");
	}

	if ( optind < argc )
	{
		o_infile = argv[optind];
		optind++;
	}
	if ( optind < argc )
	{
		printusage("Too many arguments.");
	}

	if (o_lpp)
	{
		sscanf(o_lpp,"%d",&lpp);
		if ( lpp < 1 || lpp > 512 )
		{
			sprintf(message,"Invalid lpp = %d.",lpp);
			printusage(message);
		}
	}

	sscanf(o_start,"%d:%d",&start,&end);
	if ( start < 1 )
	{
		sprintf(message,"Invalid starting pagenum = %d.",start);
		printusage(message);
	}
	if ( end != 0 && end < start )
	{
		printusage("End-page is less then start-page.");
	}

	if (o_infile)
	{
		f_in = fopen(o_infile,"r");
		if (!f_in)
		{
			sprintf(message,"Unable to open file [%s].",o_infile);
			printusage(message);
		}
	}
	else
	{
		f_in = stdin;
	}
#ifdef TESTING
printf("start=%d, end=%d, lpp=%d\n",start,end,lpp);
#endif

	rc = selectpages(f_in,stdout,start,end,lpp);

	exit(rc);
}

static int selectpages(infile,outfile,startpage,endpage,lpp)
FILE 	*infile, *outfile;
int	startpage, endpage, lpp;
{
	int	c;
	int	page, line;
	int	writerest;

	page = 1;
	line = 1;
	writerest = 0;

	while( (c = getc(infile)) != EOF )
	{
		if ( endpage != 0 && page > endpage )					/* If past endpage then stop 		*/
		{
			return(0);
		}

		if ( page >= startpage )						/* Is it time to start outputting?	*/
		{
			putc(c,outfile);
			if ( endpage == 0 )						/* If write to end then tight loop	*/
			{
				while( (c = getc(infile)) != EOF ) putc(c,outfile);	/* write out the rest of file		*/
				return(0);
			}
		}

		if ( c == '\n' )							/* Newline 				*/
		{
			line++;
			if ( line > lpp )
			{
				page++;
				line = 1;
			}
		}
		else if ( c == 0x0C )							/* Formfeed				*/
		{
			page++;
			line = 1;
		}
	}

	return(0);
}

static void printusage(char* message)
{
	if (message)
	{
		fprintf(stderr,"%s: %s\n",PROGNAME,message);
	}
	fprintf(stderr,"Usage: %s [-l lpp] start:end {infile}\n",PROGNAME);
	fprintf(stderr,"               -l lpp      The number of lines per page (default 66).\n");
	fprintf(stderr,"               start:end   The start and end page numbers to write.\n");
	fprintf(stderr,"               infile      The optional input file.\n");
	exit(1);
}
/*
**	History:
**	$Log: selectpg.c,v $
**	Revision 1.12  2003/03/20 15:01:05  gsl
**	Fix -Wall warnings
**	
**	Revision 1.11  2003/03/12 18:18:11  gsl
**	FIx -Wall warnings
**	
**	Revision 1.10  2003/02/04 20:42:49  gsl
**	fix -Wall warnings
**	
**	Revision 1.9  2003/02/04 18:50:25  gsl
**	fix copyright header
**	
**	Revision 1.8  1996/07/23 18:13:00  gsl
**	drcs update
**	
**
**
*/
