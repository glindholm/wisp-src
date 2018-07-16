static char copyright[]="Copyright (c) 1995-97 NeoMedia Technologies Inc., All rights reserved.";
static char rcsid[]="$Id:$";
/*
**	File:		ptcli.c
**
**	Project:	wisp/proctran
**
**	RCS:		$Source:$
**
**	Purpose:	???
**
**	Routines:	
**	get_cli()
**	showflags()
**
*/

#define EXT extern

#include <stdio.h>

#ifdef VMS
#include <descrip.h>
#include <climsgdef.h>
#include <ssdef.h>
#endif

#include "pgcommon.h"
#include "pgglobal.h"
#include "getopt.h"

/*
**	Structures and Defines
*/

/*
**	Globals and Externals
*/
EXT char cli_infile[200];								/* The initial input file name		*/

/*
**	Static data
*/
static char cli_listfile[200];								/* The file for conversion info listing	*/

#ifdef VMS

/* These are the descriptors for the parameters which could be returned by switches from DCL					*/
/* the cli_xxfile variables are defined in PTMAIN.C										*/

$DESCRIPTOR(cli_name,cli_infile);
$DESCRIPTOR(cli_listname,cli_listfile);

/* These are the descriptors for the various DCL switches allowed in the PROCTRAN command					*/

$DESCRIPTOR(cli_list,"LIST");
$DESCRIPTOR(cli_flag,"FLAG");
$DESCRIPTOR(cli_slink,"SLINK");
$DESCRIPTOR(cli_ccall,"CALL");
$DESCRIPTOR(cli_display,"DISPLAY");
#endif

static int show_flags;

/*
**	Static Function Prototypes
*/
static void showflags();

/*
**	ROUTINE:	get_cli()
**
**	FUNCTION:	{One line statement of function}...
**
**	DESCRIPTION:	{Full detailed description}...
**
**	ARGUMENTS:	None
**
**	GLOBALS:	None
**
**	RETURN:		None
**
**	WARNINGS:	None
**
*/
void get_cli(int argc,char *argv[])							/* Note: argc, argv used for unix code.*/
{
#ifdef VMS
	int status,i;

	status = cli$present(&cli_list);
	if (status == CLI$_PRESENT)							/* The /LIST switch is present		*/
	{
		logging = 1;								/* enable logging			*/

		status = cli$get_value(&cli_list,&cli_listname);
		if ((cli_listfile[0] == '\0') || (cli_listfile[0] == ' '))		/* No file provided though, use crt..	*/
		{
			logfile = stdout;
		}
		else									/* try to open the file			*/
		{
			cli_listfile[strpos(cli_listfile," ")] = '\0';			/* null terminate.			*/

			if (strpos(cli_listfile,".") == -1)				/* needs a .type			*/
			{
				strcat(cli_listfile,".LOG");
			}
			logfile = fopen(cli_listfile,"w");				/* open it now				*/
			if (!logfile)							/* Some error.				*/
			{
				printf("%%PROCTRAN-F-NOLOGFILE Unable to open log file.\n");
				perror(0);
				exit(1);
			}
		}
	}
	else	logging = 0;								/* Disable logging			*/

	status = cli$present(&cli_slink);
	if (status == CLI$_PRESENT)							/* The /SLINK switch is present		*/
	{
		genslink = 1;								/* enable convert to "S" type link	*/
	}
	else	genslink = 0;								/* Disable "S" type link		*/

	status = cli$present(&cli_ccall);
	if (status == CLI$_PRESENT)							/* The /CALL switch is present		*/
	{
		genccall = 1;								/* enable generate CALL verify logic 	*/
	}										/*  as a comment.			*/
	else	genccall = 0;								/* Generate CALL logic verification	*/
											/*  as compile error.			*/
	status = cli$present(&cli_display);
	if (status == CLI$_PRESENT)							/* The /DISPLAY switch is present	*/
	{
		gendisplay = 1;								/* enable generate DISPLAY of error for	*/
	}										/*  subroutines.			*/
	else	gendisplay = 0;								/* Do not generate DISPLAY statements	*/
											/*  for errors on subroutines.		*/

	status = cli$present(&cli_flag);
	if (status == CLI$_PRESENT)							/* The /FLAG switch is present		*/
	{
		show_flags = 1;
	}
	else	show_flags = 0;								/* Do not display flags set.		*/

#else /* !VMS */
	int	i,c;
	extern	char	*optarg;
	extern	int	optind,opterr;
	/*
		Set Defaults
	*/
	show_flags = 0;

	if (argc == 1)
	{
		usage(1);
		exit(1);
	}

	logging = 0;									/* Set default: Disable logging		*/
	genslink = 0;									/* Set default: Disable "S" type link	*/
	genccall = 0;									/* Set default: Generate CALL logic as	*/
	gendisplay = 0;									/* Set default: Do not generate DISPLAY	*/
	show_flags = 0;									/*  compile error			*/

	while ( (c = getopt( argc, argv, "lscfh?")) != -1 )
	{
		switch( c )
		{
			case 'l':							/* /LIST				*/
				logging = 1;						/* enable logging			*/
				break;
			case 's':							/* /SLINK				*/
				genslink = 1;						/* enable convert to "S" type link	*/
				break;
			case 'c':							/* /CALL				*/
				genccall = 1;						/* enable generate CALL logic verify 	*/
				break;							/*  as a comment.			*/
			case 'd':							/* /DISPLAY				*/
				gendisplay = 1;						/* enable generate DISPLAY logic for	*/
				break;							/*  subroutines.			*/
			case 'f':							/* SHOW ALL FLAGS IN USE		*/
				show_flags = 1;
				break;
			case 'h': case '?':
				usage(1);
				exit(1);
				break;
			default:
				printf("%%PROCTRAN-F-BADFLAG Invalid command line flag (-%c).\n",c);
				perror(0);
				exit(1);
				break;
		}
	}
#endif /* !VMS */
	strcpy( cli_infile, argv[argc-1] );						/* Get the input file name.		*/
	i = strpos(cli_infile," ");
	if (i != -1) cli_infile[i] = '\0';						/* NUll terminate			*/
	if (i = strpos(cli_infile,".") == -1)						/* needs a .type			*/
	{
		strcat(cli_infile,".wps");						/* Add default type.			*/
	}
	else										/* Make sure type in NOT .WCB		*/
	{
#ifdef unix
		i = strpos(cli_infile,"wcb");
#else /* !unix */
		i = strpos(cli_infile,"WCB");
#endif /* !unix */
		if (i != -1 && i != 0)
		{
			write_log(util,'F','R',"INVALIDTYPE","Invalid file type. Cannot use <file>.WCB");
			exit(1);
		}
	}

	if (show_flags)
	{
		showflags();
	}
}

/*
**	ROUTINE:	showflags()
**
**	FUNCTION:	{One line statement of function}...
**
**	DESCRIPTION:	{Full detailed description}...
**
**	ARGUMENTS:	None
**
**	GLOBALS:	None
**
**	RETURN:		None
**
**	WARNINGS:	None
**
*/
static void showflags()
{
	printf("The Following PROCTRAN Options are in use;\n\n");

	printf("       FLAG     VMS SWITCH		DESCRIPTION\n");
	if ( logging )
		printf("        -l	/LIST			Write all log messages.\n");
	else
		printf("     no -l	/NOLIST			No (I) level messages.\n");

	if ( genslink )
		printf("        -s	/SLINK			Generate \"S\" type links for \" \" type links.\n");
	else
		printf("     no -s	/NOSLINK		Do not change link type.\n");

	if ( genccall )
		printf("        -c	/CALL			Generate CALL logic verification as comment.\n");
	else
		printf("     no -c	/NOCALL			Generate CALL verify logic as compile error.\n");

	if ( gendisplay )
		printf("        -d	/DISPLAY		Generate DISPLAY logic when error on subroutines.\n");
	else
		printf("     no -d	/NODISPLAY		Do not generate DISPLAY logic for errors on subroutines.\n");
}

/*
**	History:
**	$Log: ptcli.c,v $
**	Revision 1.7  1997/04/21 15:03:54  scass
**	Corrected copyright.
**	
**	Revision 1.6  1996-09-12 19:14:51-04  gsl
**	Fix prototypes
**
**	Revision 1.5  1995-09-22 07:12:52-07  scass
**	Added DTMI standard headers.
**
**
**
*/
