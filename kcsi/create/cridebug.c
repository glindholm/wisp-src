static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";


#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <time.h>

#include "kcsifunc.h"

/*----
Debugging  and error trapping logic.
------*/

/*----
To enable tracing set KCSITRACE=1.
Tracing will be to $HOME/wisperr.log
------*/

static char sccsid[]="@(#)cridebug.c	1.7 9/19/94";

static int ktrace_enabled = -1;  /* -1 == not initialized */

static char work_buf[101];

static char *strnstr(char *str1,char *str2);
static void mystrncpy(char *dest,char *src,int len);
static void crid_trace(const char *str);
static void build_in_spaces(void);

/*----
Returns a pointer to the first occurrence of str2 in str1 and is case
insensitive. Returns NULL when not matched.
------*/
static char *strnstr(char *str1,char *str2)
{
	int ch1, ch2;
	char *first;

	for( ; *str1; ++str1)
		{
		first = str1;
		for( ; *str2; ++str2, ++str1)
			{
			ch1 = tolower(*str1);
			ch2 = tolower(*str2);
			if(ch1 != ch2)
				{
				str1 = first;
				break;
				}
			}
		if(*str2 == 0)
			return(first);
		}
	return(NULL);
}

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
	strunc(buf);
	kcsitrace(1,"KTRACE","COBOL",buf);
	
}

/*---
Step in and out of levels providing in and out trace print
------*/
static in_level;
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
**	Revision 1.9  2002-04-22 14:30:29-04  gsl
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
