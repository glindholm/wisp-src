#define EXT
#define INIT_COMMON
			/************************************************************************/
			/*	   PROCTRAN - Wang Procedure Language to VS COBOL Translator	*/
			/*			Copyright (c) 1990				*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/

/* PG_MAIN.C    This is the front end to the Procedure Language Translator utility.						*/
/*		It will verify that the command line is correct and also if the file requested exists.				*/
/*																*/
/* The Procedure Language Translator utility.											*/
/*	Makes it possible to generate VMS COBOL source translated from Wang Procedure Language files.				*/

#include <stdio.h>
#include <signal.h>

#ifdef VMS
#include <ssdef.h>
#include <chfdef.h>
#include <climsgdef.h>
#endif

#include "pgcommon.h"
#include "pgstruct.h"
#include "pgglobal.h"
#include "pgcblsrc.h"
#include "pgkeyw.h"
#include "pgeqtns.h"
#include "pgextrct.h"

EXT char cli_infile[STRBUFF];								/* Wang Procedure Language file name.	*/
EXT char out_fname[STRBUFF];								/* VMS COBOL generated output file name.*/

#ifndef lint
static char copyright[] = "(C) 1990,1991,1992 International Digital Scientific, Inc. All Rights Reserved.";
#endif
static char rcsid[] = "$Id:$";

static char VERSION[5];
static char MODDATE[20];

static char *sargv[50];									/* Save the argv			*/

#ifdef VMS
static unsigned long stval[5] = {SS$_ACCVIO, SS$_ABORT, 200956 ,0 ,0 };
#endif

main(argc,argv)
int argc;
char *argv[];
{
#ifdef VMS
        long handler();
#else
        void handler();
#endif
        char *p,*e,*strchr();
	char temp[256];
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
          fprintf(stderr,"\nPROCTRAN V%s (c)IDSI %s - 'proctran -h' for help\n",VERSION,MODDATE);

	for (i = 0; i < argc; i++ ) sargv[i] = argv[i];					/* Save the argv			*/
	sargv[argc] = 0;								/* Null terminate sargv			*/
	logfile = stdout;								/* Define a file for error logs		*/

											/*  Set up the exit handler for signals.*/
#ifdef VMS
	lib$establish(handler);
#endif

#ifdef unix
        signal(SIGINT,  handler );
        signal(SIGQUIT, handler );
        signal(SIGILL,  handler );
        signal(SIGEMT,  handler );
        signal(SIGBUS,  handler );
        signal(SIGSEGV, handler );
#endif

	get_cli(argc,sargv);								/* parse the CLI switches		*/
	wcbname(out_fname,cli_infile);							/* Gen the output file name.		*/

	if ((infile = fopen(cli_infile,"r")) == NULL)					/* Can't open the input file.		*/
	{
		write_log("PROCTRAN",'F',"NOTFOUND","Input file %s not found.\n",cli_infile);
	}
	else if ((outfile = fopen(out_fname,"w")) == NULL)				/* Can't open the output file.		*/
	{
		write_log("PROCTRAN",'F',"CANTOPEN","Unable to open output file %s\n",out_fname);
	}
	else
	{
		doit();									/* Do the generation.			*/
	}
#ifdef VMS										/*  Set back so uses VAX signal handler.*/
	lib$revert();
#endif
}

static wcbname(out_name,in_name)							/* Generate the output file name from	*/
char *out_name, *in_name;								/* input name.				*/
{
	int len;

	memset(out_name,' ',STRBUFF);
	len = 0;
	while (in_name[len] != ' ' && in_name[len] != '\0' && in_name[len] != '.') len++; /* Get length of input file name.	*/
	memcpy(out_name,in_name,len);							/* Copy input name to output name.	*/
#ifdef VMS
	strcpy(&out_name[len],".WCB");							/* Copy needed ext. to output name.	*/
#endif
#ifdef unix
	strcpy(&out_name[len],".wcb");							/* Copy needed ext. to output name.	*/
#endif
}

usage(prnt)
int prnt;
{
	if (!prnt) fprintf(stderr,"PROCTRAN V%s (c)IDSI %s\n",VERSION,MODDATE);

        fprintf(stderr,"\nusage:  proctran [-flags] filename\n");
	fprintf(stderr," FLAG   VMS SWITCH     DESCRIPTION\n");
	fprintf(stderr,"   -l   /LIST [=file]  Write all log messages\n");
	fprintf(stderr,"   -f   /FLAG          Show flags in use\n");
	fprintf(stderr,"   -h     -h           Show this usage text\n");
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
		fprintf(stderr,"     In file: %s line: %d\n",cli_infile,num_inlines);
	}
	fprintf(stderr,"          MODIFICATIONS ARE NEEDED!\n");
        fprintf(stderr,"\n  PROCTRAN exiting.\n");
        exit(0);
}
#endif

#ifdef VMS
long handler(sigargs,mechargs)								/* New signal handler for PROCTRAN.	*/
struct chf$signal_array *sigargs;
struct chf$mech_array *mechargs;
{
	unsigned long mstat;
	int cont;

	cont = FALSE;									/* Assume do not continue from signal.	*/
	if (sigargs->chf$l_sig_name > 400) cont = TRUE;
	mstat = lib$match_cond(&sigargs->chf$l_sig_name, &stval[0], &stval[1], &stval[2]);

	if ( cont == TRUE && mstat == 0) return(SS$_CONTINUE);				/* If no match then continue.		*/

	switch (mstat) 
	{
		case 1:
		{
			fprintf(stderr,"\n Signal ACCESS VIOLATION ... ");
			break;
		}
		case 2:
		{
			fprintf(stderr,"\n Signal ABORT ... ");
			break;
		}
		case 3:
		{
			fprintf(stderr,"\n  New switch added.  Do a SET COMMAND PROCTRAN with new PROCTRAN.CLD or reboot.\n");
			break;
		}
		default:								/* Else, display message.		*/
		{
			fprintf(stderr,"\n  Unknown SIGNAL (%x) detected ... ",sigargs->chf$l_sig_name);
			break;
		}
	}

	if (mstat != 3)
	{
		if (num_outlines > 13)
		{
			fprintf(stderr,"writing file.\n");
			fprintf(stderr,"     Out file: %s line: %d\n",out_fname,num_outlines);
		}
		else
		{
			fprintf(stderr,"reading file.\n");
			fprintf(stderr,"     In file: %s line: %d\n",cli_infile,num_inlines);
		}

		fprintf(stderr,"     Contact IDSI with sample code OR comment line and try again!\n");
	}

        fprintf(stderr,"\n  PROCTRAN exiting.\n");
	sys$unwind(mechargs->chf$l_mch_savr0);						/* Unwind the call stack by the depth 	*/
        exit(mstat);									/* between established and when condition*/
}											/* was signaled.			*/
#endif
