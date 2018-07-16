			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

#ifdef MSDOS

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>

#include "idsistd.h"

#define CAC	( argv[optind][cur_char] )

char	*optarg;
int	optind;
int	opterr=1;

static	int	cur_char = 0;
static	int	restart = 1;


/*
**	Routine:	getopt()
**
**	Function:	To MSDOS emulation of unix function.
**
**	Description:	This routine is used to parse command line options.
**			It accept both "-a" and "/a" style options.
**			An option of "--" or "/-" will end option processing, a -1 will be returned and optind will point
**			to the next argument.
**
**			See the unix manual getopt(3) for full description.
**
**	Arguments:
**	argc		The standard argument count.
**	argv		The standard argument array.
**	flags		List of single character options. A char followed by a ':' is
**			expected to have an arguments (flags="af:q" will handle "-a -f file -q" syntax).
**			The chars ':', ' ', and '-' are not allowed.
**
**	Globals:
**	optarg		Set to point at the argument of a option ':'.
**	optind		The index to argv[].  It starts at 1 and is incremented as getopt() is called. When -1 is returned it
**			will be positioned to the next argument in argv[].
**	opterr		1=print error. 0=Don't print error.
**
**	Return:		The option character
**	-1		No more options
**	?		Unrecognized option
**
**	Warnings:	If you call getopt() after it returns -1 it will start again. This is NOT documented
**			in the unix version.
**
**	History:	
**	03/24/93	Written by GSL
**
*/
int	getopt( argc, argv, flags)
int	argc;
char	*argv[];
char	*flags;
{
	int	ret_code;
	char	*flg_ptr;

	ret_code = 0;

	if (restart)
	{
		optind = 1;
		cur_char = 0;
		restart = 0;
	}

	if (cur_char != 0)
	{
		++cur_char;
		if( CAC == (char)0 )
		{
			++optind;
			cur_char = 0;
		}
	}

	if (optind >= argc)
	{
		restart = 1;
		return(-1);
	}

	if (cur_char == 0)
	{
		if ((CAC != '-') && (CAC != '/'))				/* Lead char can be '-' or '/'			*/
		{
			restart = 1;						/* Stop on first non-option argument		*/
			return(-1);
		}

		cur_char = 1;

		if (CAC == '-')							/* Special case "--" means end of options	*/
		{
			++optind;						/* Skip past the "--"				*/
			restart = 1;
			return(-1);
		}
	}

	if (CAC == (char)0 || CAC == ' ' || CAC == ':')				/* Not valid options so stop			*/
	{
		restart = 1;
		return(-1);
	}

	if ( NULL == ( flg_ptr = strchr ( flags, CAC ) ) )
	{
		if (opterr) fprintf(stderr,"%s: illegal option -- %c",argv[0],CAC);
		return('?');
	}

	ret_code = CAC;

	if (*(flg_ptr+1) == ':')
	{
		int	save_cur_char;

		save_cur_char = cur_char;

		++cur_char;
		if( CAC == (char)0 )						/* "program -f file"				*/
		{
			if (optind+1 >= argc)					/* No argument follows				*/
			{
				if (opterr) fprintf(stderr,"%s: option requires an argument -- %c",argv[0],ret_code);
				ret_code = '?';
				cur_char = save_cur_char;			/* Restore position				*/
			}
			else
			{
				++optind;					/* Skip to option argument			*/
				cur_char = 0;
				optarg = &CAC;					/* Set up optarg to point to argument		*/
				++optind;					/* Point to next arg				*/
			}
		}
		else								/* "program -ffile" this syntax will be allowed	*/
		{
			optarg = &CAC;
			++optind;						/* Set up for next call				*/
			cur_char = 0;
		}
	}

	return(ret_code);
}	

#else	/* MSDOS */
static int dummy_getopt;
#endif
