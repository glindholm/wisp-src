/*
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** $Id:$
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
** $Source:$
** $Author: gsl $
** $Date:$
** $Revision:$
*/

/*
**	File:		getopt.c
**
**	Project:	wisp/common
**
**	RCS:		$Source:$
**
**	Purpose:	Get command line options
**
**	Routines:	
**	getopt()       
*/

#if defined(WIN32)

/*
**	Includes
*/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>

#include "idsistd.h"
#include "getopt.h"

/*
**	Structures and Defines
*/
#define CAC	( argv[optind][cur_char] )

/*
**	Globals and Externals
*/
char	*optarg;
int	optind;
int	opterr=1;

/*
**	Static data
*/
static	int	cur_char = 0;
static	int	restart = 1;

/*
**	Static Function Prototypes
*/


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
int	getopt( int argc, char *const argv[], const char *flags)
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

#endif /*  WIN32 */
/*
**	History:
**	$Log: getopt.c,v $
**	Revision 1.10  2003/01/31 21:40:59  gsl
**	Fix -Wall warnings
**	
**	Revision 1.9  2003/01/31 19:26:33  gsl
**	Fix copyright header
**	
**	Revision 1.8  2002/07/18 21:04:22  gsl
**	Remove MSDOS code
**	
**	Revision 1.7  1997/02/17 15:49:07  gsl
**	Fixed prototype so same as ansi C standard
**	
**	Revision 1.6  1996-09-12 19:11:11-04  gsl
**	change WINNT to WIN32
**
**	Revision 1.5  1996-06-21 09:26:12-07  gsl
**	Update for NT
**
**	Revision 1.7  1996-01-02 07:24:39-08  gsl
**	Changed copyright date.
**
**
**
*/

