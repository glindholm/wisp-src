/*
******************************************************************************
**
** KCSI - King Computer Services Inc.
**
** 
******************************************************************************
*/


#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <time.h>
#include <ctype.h>

#include "kcsifunc.h"

/*----
Debugging  and error trapping logic.
------*/

/*----
To enable tracing set KCSITRACE=1.
Tracing will be to $HOME/wisperr.log
------*/


static int ktrace_enabled = -1;  /* -1 == not initialized */

static void mystrncpy(char *dest,char *src,int len);
static void build_in_spaces(void);

/*----
Copies src to dest for len bytes or until a NUL is encountered, and
appends a NUL.
------*/

static void mystrncpy(char *dest,char *src,int len)
{
	while(len--)
		{
		if(*src == 0)
			break;
		*dest++ = *src++;
		}
	*dest = 0;
}


/*----
Called by each of the vers modules rptvers, ctlvers etc.
Sets debugging and tracing levels from environment variables
------*/
void init_crid_debug(void)
{
	ktrace_enabled = (kcsi_tracelevel() != 4);
}

/*----
Called from COBOL programs and passed an 80n byte value, this 
routine will print the string to the log file KCSITRACE=1
------*/
void KTRACE(char *str)
{
	char buf[81];

	if(ktrace_enabled == -1)
	{
		init_crid_debug();
	}

	if(ktrace_enabled != 1)	return;

	mystrncpy(buf,str,80);
	KCSI_strunc(buf);
	kcsitrace(1,"KTRACE","COBOL",buf);
	
}

/*---
Step in and out of levels providing in and out trace print
------*/
static int in_level;
static char in_spaces[101];

void static build_in_spaces(void)
{
	int level;
	level = in_level;
	strcpy(in_spaces,"");
	while(level)
		{
		strcat(in_spaces,"  ");
		--level;
		if(level < 0)
			break;
		}
}

void crid_func_trace_in(char *func)
{
	if(ktrace_enabled != 1)	return;

	++in_level;
	build_in_spaces();

	kcsitrace(1,"FUNC","ENTER","%2d%s-->%s()",in_level,in_spaces,func);
}

void crid_func_trace_out(char *func)
{
	if(ktrace_enabled != 1)	return;

	build_in_spaces();

	kcsitrace(1,"FUNC","EXIT ","%2d%s<--%s()",in_level,in_spaces,func);

	--in_level;
	if(in_level < 0)
		in_level = 0;
}

/*
**	History:
**	$Log: cridebug.c,v $
**	Revision 1.12  2003/02/05 15:23:59  gsl
**	Fix -Wall warnings
**	
**	Revision 1.11  2003/02/04 19:19:09  gsl
**	fix header
**	
**	Revision 1.10  2002/07/25 15:20:30  gsl
**	Globals
**	
**	Revision 1.9  2002/04/22 18:30:29  gsl
**	Rework to use kcsitrace()
**	
**	Revision 1.8  2002-04-19 17:00:05-04  gsl
**	Remove the crid_trace_level
**	open kcsierr.log in append mode
**	close after each write
**
**	Revision 1.7  1998-01-20 11:45:34-05  gsl
**	fix time reference to be type time_t
**
**	Revision 1.6  1996-10-09 12:47:56-04  gsl
**	Add include stdlib
**
**	Revision 1.5  1996-09-17 16:34:04-07  gsl
**	drcs update
**
**
**
*/
