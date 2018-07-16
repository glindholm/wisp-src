static char copyright[]="Copyright (c) 1995-1998 NeoMedia Technologies, All rights reserved.";
static char rcsid[]="$Id:$";


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
cob_file_context *get_curr_cob_context();


void write_tlog(tokptr,facil,sever,mess,lform,p0,p1,p2,p3,p4,p5,p6,p7)			/* write a log line to the current log	*/
TOKEN *tokptr;
char *facil,sever,*mess;								/* facility, severity, message		*/
char *lform,*p0,*p1,*p2,*p3,*p4,*p5,*p6,*p7;						/* The format and parms for the text	*/
{
	char 	*ptr;
	int	num;
	char	where[80];

	if (nowarnings && sever == 'W') return;

	if (tokptr && tokptr->context)
	{
		ptr = context_infile_name(tokptr->context);
		num = context_infile_count(tokptr->context);
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


	if (logging)
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

write_log(facil,sever,mess,lform,p0,p1,p2,p3,p4,p5,p6,p7)				/* write a log line to the current log	*/
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
**	Revision 1.16  1999-09-07 10:41:32-04  gsl
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
