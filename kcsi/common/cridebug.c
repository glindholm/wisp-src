static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";


#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include "kcsifunc.h"

/*----
Debugging  and error trapping logic.
------*/

/*----
To trace C routines set the environment variable CRIDTRACE to a number.
The number is equal to the depth of tracing wanted.
Trace messages are placed in the kcsierr.log.
------*/

/*----
To trace COBOL routines set the environment variable CRIDEBUG=cobol
------*/

static char sccsid[]="@(#)cridebug.c	1.7 9/19/94";

int crid_trace_level;

static int ktrace_enabled = -1;

static FILE *dbgf;

static char work_buf[101];

static char *strnstr(char *str1,char *str2);
static void mystrncpy(char *dest,char *src,int len);
static void set_debug_level(void);
static void set_trace_level(void);
static void crid_trace(char *date,char *str);
static void build_in_spaces(void);
static char *sys_md(void);
static char *sys_hms(void);
static char *sys_date_time(void);

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
	set_debug_level();
	set_trace_level();
}

static void set_debug_level(void)
{
	char *ptr;

	ktrace_enabled = 0;
	ptr = getenv("CRIDEBUG");
	if(ptr)
		{
		if(*ptr)
			{
			if(strnstr(ptr,"cobol"))
				ktrace_enabled = 1;
			}
		}

}

static void set_trace_level(void)
{
	char *ptr;

	crid_trace_level=0;
	ptr = getenv("CRIDTRACE");
	if(ptr)
		sscanf(ptr,"%d",&crid_trace_level);
	
}

/*----
Print function and function level (not used much) as
func_in and func_out routines are more thorough
------*/
void crid_func_trace(char *func,int level)
{
	if(level > crid_trace_level)
		return;
	sprintf(work_buf,"%s()",func);
	crid_str_trace(work_buf);
}
/*----
Print if tracing is on
------*/
void crid_str_trace(char *str)
{
	if(!crid_trace_level)
		return;
	crid_str(str);
}

/*----
Called from COBOL programs and passed an 80n byte value, this 
routine will print the string to the kcsierr.log file if
CRIDEBUG=cobol
------*/
void KTRACE(char *str)
{
	char buf[81];

	if(ktrace_enabled == -1)
		set_debug_level();

	if(!(ktrace_enabled))
		return;
	mystrncpy(buf,str,80);
	strunc(buf);
	crid_str(buf);
}

void crid_str(char *str)
{
	crid_trace("",str);
}
/*----
Always print regardless of trace or debug level
Used by cvt_error() routine in rcvt.c
------*/
void crid_error_trace(char *str)
{
	char ebuf[101];

	char *sys_md(), *sys_hms();
	

	sprintf(ebuf,"%s %s ",sys_md(),sys_hms());
	
	crid_trace(ebuf,str);

}

/*----
Always print regardless of trace level
------*/
static void crid_trace(char *date,char *str)
{
	if(!dbgf)
		dbgf = fopen("kcsierr.log","w");
	fprintf(dbgf,"%s%s\n",date,str);
	fflush(dbgf);
}

/*---
Step in and out of levels providing in and out trace print
------*/
static in_level;
static char in_spaces[101];

void crid_func_trace_in(char *func)
{

	if(!crid_trace_level)
		return;
	++in_level;
	if(in_level > crid_trace_level)
		return;
	build_in_spaces();
	sprintf(work_buf,"%2d%s-->%s()",in_level,in_spaces,func);
	crid_str_trace(work_buf);
}

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

void crid_func_trace_out(char *func)
{
	if(!crid_trace_level)
		return;
	if(in_level <= crid_trace_level)
		{
		build_in_spaces();
		sprintf(work_buf,"%2d%s<--%s()",in_level,in_spaces,func);
		crid_str_trace(work_buf);
		}
	--in_level;
	if(in_level < 0)
		in_level = 0;
}

static char *sys_md(void)
{
	static 	char md[]="Jan 23";
	char *atime;
	char *sys_date_time();

	atime = sys_date_time();
	memcpy(md,&atime[4],6);
	md[6]=0;
	return(md);	
}

static char *sys_hms(void)
{
	static 	char hms[]="00:00:00";
	char *atime;
	char *sys_date_time();

	atime = sys_date_time();
	memcpy(hms,&atime[11],8);
	hms[8] = 0;
	return(hms);	
}

static char *sys_date_time(void)
{
	long some_time;

	time(&some_time);
	return((char*) ctime(&some_time));
}
/*
**	History:
**	$Log: cridebug.c,v $
**	Revision 1.6  1996-10-09 12:47:56-04  gsl
**	Add include stdlib
**
**	Revision 1.5  1996-09-17 16:34:04-07  gsl
**	drcs update
**
**
**
*/
