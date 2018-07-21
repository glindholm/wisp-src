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



#include <stdio.h>
#include <ctype.h>

#define WRITE_LOG

#define EXT extern
#include "wisp.h"
#include "wispfile.h"
#include "token.h"
#include "lines.h"
#include "input.h"

char *context_infile_name();

static int highest_serverity = SEVER_SUCCESS;

static void got_serverity(char sever)
{
	int sever_num = SEVER_SUCCESS;
	switch(sever)
	{

	case 'W': /* Warning */
		sever_num = SEVER_WARNING;
		break;
	case 'E': /* Error */
		sever_num = SEVER_ERROR;
		break;
	case 'F': /* Fatal */
		sever_num = SEVER_FATAL;
		break;
	case 'S': /* Severe */
		sever_num = SEVER_FATAL;
		break;
	}

	if (sever_num > highest_serverity)
	{
		highest_serverity = sever_num;
	}

}

int get_highest_serverity()
{
	return highest_serverity;
}

void write_tlog(tokptr,facil,sever,mess,lform,p0,p1,p2,p3,p4,p5,p6,p7)			/* write a log line to the current log	*/
TOKEN *tokptr;
char *facil,sever,*mess;								/* facility, severity, message		*/
char *lform,*p0,*p1,*p2,*p3,*p4,*p5,*p6,*p7;						/* The format and parms for the text	*/
{
	char 	*ptr;
	int	num;
	char	where[80];

	got_serverity(sever);

	if (opt_nowarnings && sever == 'W') return;

	if (tokptr && tokptr->context)
	{
		ptr = context_infile_name(tokptr->context);
		if (0 == tokptr->line)
		{
			num = context_infile_count(tokptr->context);
		}
		else
		{
			num = tokptr->line;
		}
		if (tokptr->column)
		{
			sprintf(where,"(%s:%d.%d)",ptr,num,tokptr->column);
		}
		else
		{
			sprintf(where,"(%s:%d)",ptr,num);
		}
	}
	else if (curr_cob_context)
	{
		ptr = context_infile_name(curr_cob_context);
		num = context_infile_count(curr_cob_context);
		sprintf(where,"(%s:%d)",ptr,num);
	}
	else
	{
		where[0] = (char)0;
	}


	if (opt_logging)
	{
		if (sever != ' ')							/* if severity set to space, skip	*/
		     fprintf(logfile,"%%%s-%c-%s %s ",facil,sever,mess,where);		/* write facility, severity, message	*/
		fprintf(logfile,lform,p0,p1,p2,p3,p4,p5,p6,p7);				/* if logging, write it			*/
		fprintf(logfile,"\n");
	}
	else if (sever != 'I' && sever != ' ')						/* if not informational only, report it	*/
	{
		switch(sever)
		{
		case 'T': /* Trace */
		case 'I': /* Informational */
		case 'W': /* Warning */
		case 'E': /* Error */
		case 'F': /* Fatal */
		case 'S': /* Severe */
			printf("%%%s-%c-%s %s ",facil,sever,mess,where);		/* write facility, severity, message	*/
			break;
		case 'M': /* Message */
			printf("%%WISP ");
			break;
		}
		printf(lform,p0,p1,p2,p3,p4,p5,p6,p7);					/* and the text				*/
		printf("\n");
	}
}

int write_log(facil,sever,mess,lform,p0,p1,p2,p3,p4,p5,p6,p7)				/* write a log line to the current log	*/
char *facil,sever,*mess;								/* facility, severity, message		*/
char *lform,*p0,*p1,*p2,*p3,*p4,*p5,*p6,*p7;						/* The format and parms for the text	*/
{
	write_tlog(NULL,facil,sever,mess,lform,p0,p1,p2,p3,p4,p5,p6,p7);
	return 0;
}

static int linelength(const char* line)
{
	const char *ptr;
	int	linelen;

	ptr = strchr(line,'\n');
	if ( ptr ) linelen = ptr - line;
	else	   linelen = strlen(line);
	return(linelen);
}

int iscomment(const char* the_line)
{
	if ((linelength(the_line) > 6) && (the_line[6] == '*' || the_line[6] == '/')) return(1);
	return(0);
}

/*
**	History:
**	$Log: wt_io.c,v $
**	Revision 1.22  2005/12/02 15:22:47  gsl
**	Keep track of the highest severity level reported.
**	Ensure an non-zero exit status if severity is fatal or higher.
**	
**	Revision 1.21  2003/12/03 16:18:48  gsl
**	Fix so native screen fields and screen sections don't get generated in a copybook file.
**	
**	Revision 1.20  2003/02/28 21:49:05  gsl
**	Cleanup and rename all the options flags opt_xxx
**	
**	Revision 1.19  2003/02/04 18:43:32  gsl
**	fix -Wall warnings
**	
**	Revision 1.18  2003/02/04 17:33:19  gsl
**	fix copyright header
**	
**	Revision 1.17  2002/08/13 16:10:44  gsl
**	On an error report the line number from the token if available
**	
**	Revision 1.16  1999/09/07 14:41:32  gsl
**	fix return types
**	
**	Revision 1.15  1998-03-27 13:37:07-05  gsl
**	fix warnings
**
**	Revision 1.14  1998-03-23 13:11:57-05  gsl
**	FIx proto types.
**	Make iscomment() const
**
**	Revision 1.13  1998-03-17 16:56:55-05  gsl
**	Removed the OLD get_line() hold_line() logic.
**
**	Revision 1.12  1997-02-24 09:58:14-05  gsl
**	Removed the mod_code[] processing. This was remnants of earlier
**	logic which has been removed.  A mod code greater then 11 chars was
**	overwritting memory onto twiddle_count and causing the "twiddle" error.
**
**	Revision 1.11  1996-08-30 21:56:21-04  gsl
**	drcs update
**
**
**
*/
