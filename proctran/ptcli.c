/*
******************************************************************************
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
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
******************************************************************************
*/

/*
**	File:		ptcli.c
**
**	Project:	wisp/proctran
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
	int	i,c;

	/*
	extern	char	*optarg;
	extern	int	optind;
	extern	int	opterr;
	*/

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
	strcpy( cli_infile, argv[argc-1] );						/* Get the input file name.		*/
	i = strpos(cli_infile," ");
	if (i != -1) cli_infile[i] = '\0';						/* NUll terminate			*/
	if ((i = strpos(cli_infile,".")) == -1)						/* needs a .type			*/
	{
		strcat(cli_infile,".wps");						/* Add default type.			*/
	}
	else										/* Make sure type in NOT .WCB		*/
	{
		i = strpos(cli_infile,"wcb");
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
**	Revision 1.11  2003/03/20 15:01:05  gsl
**	Fix -Wall warnings
**	
**	Revision 1.10  2003/02/05 21:15:03  gsl
**	fix -Wall warnings
**	
**	Revision 1.9  2003/02/04 18:57:00  gsl
**	fix copyright header
**	
**	Revision 1.8  2002/06/26 01:42:50  gsl
**	Remove VMS code
**	
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
