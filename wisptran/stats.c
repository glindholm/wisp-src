/*
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
** NOTICE:
** Confidential, unpublished property of Shell Stream Software LLC.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of Shell Stream Software LLC
** is strictly prohibited.
** 
*/


/*
**	File:		stats.c
**
**	Purpose:	Routines to get wisp stats.
**
**	Routines:	
**	foo()		Sample ...
**
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


/*
**	History:
**	$Log: stats.c,v $
**	Revision 1.7  2003/02/05 15:40:13  gsl
**	Fix copyright headers
**	
**	Revision 1.6  2003/02/04 17:33:19  gsl
**	fix copyright header
**	
**	Revision 1.5  1996/08/31 01:56:10  gsl
**	drcs update
**	
**
**
*/
