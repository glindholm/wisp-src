/*
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
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
**	File:		wt_debug.c.c
**
**	Purpose:	Debug info printing routines.
**
**			Level 1:	Raw input lines.
**			Level 2:	Input line stream after conditional code and cleanup
**			Level 3:	Tokens
**
**	Routines:	
**	debug_set_levels()	Set which debug levels are active.
**	debug_level()		Report if a debug level is active.
**	debug3_print_token()	Level 3: Print a token.
**	debug1_print_line()	Level 1: Print a line.
**	debug2_print_line()	Level 2: Print a line.
**	debug3_print_line()	Level 3: Print a line.
**	debug_std_print_line()	Standard line printing routine.
**
**
**	History:
**	05/17/93	Written by GSL
**
*/

#include <stdio.h>
#include <string.h>

#include "token.h"
#include "node.h"
#include "wispfile.h"
#include "wisp.h"

static char	the_debug_levels[] = "NNNNNNNNNN";
static int debug_std_print_line();

int debug_set_level(levels)
char	*levels;
{
	int	i;

	for(i=0; levels[i]; i++)
	{
		if (levels[i] >= '0' && levels[i] <= '9')
		{
			the_debug_levels[levels[i] - '0'] = 'Y';
		}
	}
	return 0;
}

int debug_level(int level)
{
	if (level < (int)strlen(the_debug_levels) && the_debug_levels[level] == 'Y')
		return(1);
	return(0);
}

int debug3_print_token(tokptr)
TOKEN *tokptr;
{
	static int first = 1;
	static int debug;

	if (first)
	{
		debug = debug_level(3);
		first = 0;
	}

	if (debug)
	{
		char	*infile;
		cob_file_context *the_context;

		the_context = (cob_file_context *)tokptr->context;

		if (the_context && the_context->infile && the_context->infile->a_file)
		{
			infile = the_context->infile->a_file->name;
		}
		else
		{
			infile = "(UNKNOWN)";
		}
		printf("%5d %24.24s %5d >%3d %c %-15s [%s]\n", tokptr->absline, infile, tokptr->line, tokptr->column,
			(tokptr->column_fixed)?'F':' ', token_type_mess(tokptr), tokptr->data);
		fflush(stdout);
	}
	return 0;
}

int debug1_print_line(the_line)
char *the_line;
{
	static int first = 1;
	static int debug;

	if (first)
	{
		debug = debug_level(1);
		first = 0;
	}

	if (debug)
	{
		debug_std_print_line(the_line);
	}
	return 0;
}

int debug2_print_line(the_line)
char *the_line;
{
	static int first = 1;
	static int debug;

	if (first)
	{
		debug = debug_level(2);
		first = 0;
	}

	if (debug)
	{
		debug_std_print_line(the_line);
	}
	return 0;
}

int debug3_print_line(the_line,absline,linenum,the_context)
char *the_line;
int	absline;
int	linenum;
cob_file_context *the_context;
{
	static int first = 1;
	static int debug;
	char	*infile;

	if (first)
	{
		debug = debug_level(3);
		first = 0;
	}

	if (debug)
	{
		if (the_context && the_context->infile && the_context->infile->a_file)
		{
			infile = the_context->infile->a_file->name;
		}
		else
		{
			infile = "(UNKNOWN)";
		}
		printf("%5d %24.24s %5d :%s",absline, infile, linenum, the_line);
		if (!strchr(the_line,'\n'))
		{
			printf("\n");
		}
		fflush(stdout);
	}
	return 0;
}

static int debug_std_print_line(the_line)
char *the_line;
{
	printf("%5d %24.24s %5d :%s",in_line_count(), curr_cob_context->infile->a_file->name, 
						      curr_cob_context->infile->line_count, the_line);
	fflush(stdout);
	return 0;
}

int debug4_print_statement(NODE the_statement)
{
	static int first = 1;
	static int debug;

	if (first)
	{
		debug = debug_level(4);
		first = 0;
	}

	if (debug)
	{
		if (!the_statement)
		{
			printf("[empty statement]\n");
		}
		else
		{
			printf("*----------------------------------------------------------------------------------*\n");
			debug_print_tree(the_statement,0);
		}
	}
	return 0;
}

int debug_print_tree(NODE the_tree, int depth)
{
	if (!the_tree) return 0;

	if (the_tree->token)
	{
		char	*infile;
		cob_file_context *the_context;

		the_context = (cob_file_context *)the_tree->token->context;

		if (the_context && the_context->infile && the_context->infile->a_file)
		{
			infile = the_context->infile->a_file->name;
		}
		else
		{
			infile = "(UNKNOWN)";
		}
		printf("%5d %24.24s %5d >%3d %c %-15s %s[%s]\n", 
			the_tree->token->absline, 
			infile,
			the_tree->token->line, 
			the_tree->token->column,
			(the_tree->token->column_fixed) ? 'F':' ', 
			token_type_mess(the_tree->token), 
			(depth) ? "          ":"",
			the_tree->token->data);
		fflush(stdout);
	}

	debug_print_tree(the_tree->down,depth+1);
	debug_print_tree(the_tree->next,depth);
	return(0);
}

/*
**	History:
**	$Log: wt_debug.c,v $
**	Revision 1.9  2003/02/04 18:02:20  gsl
**	fix -Wall warnings
**	
**	Revision 1.8  2003/02/04 17:33:19  gsl
**	fix copyright header
**	
**	Revision 1.7  1996/10/09 16:31:19  gsl
**	Fix warnings
**	
**	Revision 1.6  1996-08-30 18:56:15-07  gsl
**	drcs update
**
**
**
*/
