			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
**	File:		stats.c
**
**	Purpose:	Routines to get wisp stats.
**
**	Routines:	
**	foo()		Sample ...
**
**
**	History:
**	mm/dd/yy	Written by ...
**
*/

static int the_in_line_count = 0;
int in_line_count()
{
	return(the_in_line_count);
}

int increment_in_line_count()
{
	return(++the_in_line_count);
}

static int the_out_line_count = 0;
int out_line_count()
{
	return(the_out_line_count);
}

int increment_out_line_count()
{
	return(++the_out_line_count);
}

static int the_comment_line_count = 0;
int comment_line_count()
{
	return(the_comment_line_count);
}

int increment_comment_line_count()
{
	return(++the_comment_line_count);
}

static int the_main_line_count = 0;
int main_line_count()
{
	return(the_main_line_count);
}

int increment_main_line_count()
{
	return(++the_main_line_count);
}


