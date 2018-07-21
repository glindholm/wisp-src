/*
******************************************************************************
** Copyright (c) Shell Stream Software LLC. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
** 
******************************************************************************
*/

static char rcsid[]="$Id: ptmain.c,v 1.19 2010/01/10 16:04:27 gsl Exp $"; /* USED BELOW */

/*
**	File:		ptmain.c
**
**	Purpose:	This is the front end to the Procedure Language Translator utility.
**			It will verify that the command line is correct and also if the file requested exists
**
**			Makes it possible to generate VMS COBOL source translated from Wang Procedure Language files.
**
**	Routines:	main()		Initial entry point of utility.
**			wcbname()	Generate the output file name from input name.
**			usage()		Display the usage parameters for the utility.
**			handler()	Define the exit handler.  One for UNIX and one for VMS.
**
**
**
*/

#define EXT
#define INIT_COMMON

#include <stdio.h>
#include <signal.h>


#include "pgcommon.h"
#include "pgstruct.h"
#include "pgglobal.h"
#include "pgcblsrc.h"
#include "pgkeyw.h"
#include "pgeqtns.h"
#include "pgextrct.h"

EXT char cli_infile[STRBUFF];								/* Wang Procedure Language file name.	*/
EXT char out_fname[STRBUFF];								/* VMS COBOL generated output file name.*/

static char VERSION[5];
static char MODDATE[20];

static char *sargv[50];									/* Save the argv			*/
static void wcbname(char* out_name, char* in_name);					/* Generate the output file name from	*/


int main(argc,argv)
int argc;
char *argv[];
{
        void handler();
        char *p,*e;
	int i;

        memset(VERSION,0,sizeof(VERSION));
        p=strchr(rcsid,' ');
        p=strchr(++p,' ');
        e=strchr(++p,' ');
        
        memcpy(VERSION,p,e-p);
        memset(MODDATE,0,sizeof(MODDATE));
        p=strchr(++e,' ');
        memcpy(MODDATE,e,p-e);

        setbuf(stderr,NULL);
        
        if (argc==2 && argv[1][0]=='-' && (argv[1][1]=='?'||argv[1][1]=='h')) usage(0);
        else
          fprintf(stderr,"\nPROCTRAN V%s (c) Shell Stream Software LLC %s - 'proctran -h' for help\n",VERSION,MODDATE);

	for (i = 0; i < argc; i++ ) sargv[i] = argv[i];					/* Save the argv			*/
	sargv[argc] = 0;								/* Null terminate sargv			*/
	logfile = stdout;								/* Define a file for error logs		*/

											/*  Set up the exit handler for signals.*/

#ifdef unix
	if (getenv("DBPROC"))
	{
	        signal(SIGINT,  SIG_DFL );
        	signal(SIGQUIT, SIG_DFL );
	        signal(SIGILL,  SIG_DFL );
	        signal(SIGBUS,  SIG_DFL );
        	signal(SIGSEGV, SIG_DFL );
	}
	else
	{
	        signal(SIGINT,  handler );
        	signal(SIGQUIT, handler );
	        signal(SIGILL,  handler );
	        signal(SIGBUS,  handler );
        	signal(SIGSEGV, handler );
	}
#endif

	get_cli(argc,sargv);								/* parse the CLI switches		*/
	wcbname(out_fname,cli_infile);							/* Gen the output file name.		*/

	if ((infile = fopen(cli_infile,"r")) == NULL)					/* Can't open the input file.		*/
	{
		write_log(util,'F',' ',"NOTFOUND","Input file %s not found.\n",cli_infile);
	}
	else if ((outfile = fopen(out_fname,"w")) == NULL)				/* Can't open the output file.		*/
	{
		write_log(util,'F',' ',"CANTOPEN","Unable to open output file %s\n",out_fname);
	}
	else
	{
          	fprintf(stderr,"  proctran of file:  <<%s>>\n\n",cli_infile);
		init_literals();							/* Initialize literals used.		*/
		doit();									/* Do the generation.			*/
	}
	return 0;
}

static void wcbname(char* out_name, char* in_name)					/* Generate the output file name from	*/
											/* input name.				*/
{
	int len;

	memset(out_name,' ',STRBUFF);
	len = 0;
	while (in_name[len] != ' ' && in_name[len] != '\0' && in_name[len] != '.') len++; /* Get length of input file name.	*/
	memcpy(out_name,in_name,len);							/* Copy input name to output name.	*/
	strcpy(&out_name[len],".wcb");							/* Copy needed ext. to output name.	*/
}

void usage(int prnt)
{
	if (!prnt) fprintf(stderr,"PROCTRAN V%s (c) Shell Stream Software LLC %s\n",VERSION,MODDATE);

        fprintf(stderr,"\nusage:  proctran [-flags] filename\n");
	fprintf(stderr," FLAG   VMS SWITCH     DESCRIPTION\n");
	fprintf(stderr,"   -l   /LIST [=file]  Write all log messages\n");
	fprintf(stderr,"   -s   /SLINK         Generate \"S\" type link for \" \" type link\n");
	fprintf(stderr,"   -c   /CALL          Generate CALL verification as comment\n");
	fprintf(stderr,"   -f   /FLAG          Show flags in use\n");
	fprintf(stderr,"   -h     -h           Show this usage text\n");
	fprintf(stderr,"\nSet shell variable DBPROC=yes to stop signal trapping.\n");
	fprintf(stderr,"\n");
        exit(1);
}

#ifdef unix
void handler()
{
	fprintf(stderr,"\n  SIGNAL detected ... ");
	if (num_outlines > 13)
	{
		fprintf(stderr,"writing file.\n");
		fprintf(stderr,"     Out file: %s line: %d\n",out_fname,num_outlines);
	}
	else
	{
		fprintf(stderr,"reading file.\n");
		fprintf(stderr,"     In file: %s line: %d\n",cli_infile,num_lineins);
	}
	fprintf(stderr,"          MODIFICATIONS ARE NEEDED!\n");
        fprintf(stderr,"\n  PROCTRAN exiting.\n");
        exit(0);
}
#endif


/*
**	History:
**	$Log: ptmain.c,v $
**	Revision 1.19  2010/01/10 16:04:27  gsl
**	Shell Stream
**	
**	Revision 1.18  2003/02/05 21:15:03  gsl
**	fix -Wall warnings
**	
**	Revision 1.17  2003/02/05 15:50:12  gsl
**	Fix copyright headers
**	
**	Revision 1.16  2003/02/05 15:40:14  gsl
**	Fix copyright headers
**	
**	Revision 1.15  2003/02/04 19:19:09  gsl
**	fix header
**	
**	Revision 1.14  2003/02/04 18:57:00  gsl
**	fix copyright header
**	
**	Revision 1.13  2002/09/04 18:10:18  gsl
**	LINUX
**	
**	Revision 1.12  2002/06/26 01:42:50  gsl
**	Remove VMS code
**	
**	Revision 1.11  1997/04/21 15:37:46  scass
**	Corrected double copyright definition.
**	
**	Revision 1.10  1997-04-21 11:16:35-04  scass
**	Corrected copyright.
**
**	Revision 1.9  1996-12-12 13:31:43-05  gsl
**
**	Revision 1.8  1996-09-13 08:56:58-07  gsl
**	Remove explicit strchr() def
**
**	Revision 1.7  1996-09-12 16:17:29-07  gsl
**	fix prototypes
**
**
**
*/
