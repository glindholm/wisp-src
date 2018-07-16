static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";

#include <ctype.h>
#include "kcsifunc.h"

static char sccsid[]="@(#)rcvs.c	1.3 10/28/93";

/*----
Comparison and extraction routines.
------*/
/*----
Compare a field at a position in a record for len
with memory in m1.
------*/
int fieldeq(char *record,char *base,int pos,int len,int type,int dec,char* m1)
{
	return(!(memcmp(record + pos,m1,len)));
}
/*----
Return a single character from a record at position pos.
------*/
int charat(char *record,char *base,int pos,int len,int type,int dec)
{
	return(record[pos]);
}
/*----
ASCII at a field position in a record for length converted to
integer.
------*/
int atoifield(char *record,char *base,int pos,int len,int type,int dec)
{
	return(atoilen(record + pos,len));
}
void strunc(char* dest)
{
	char *end;

	end = dest + strlen(dest);
	if(end == dest)
		return;
	--end;
	while((*end == ' ') && (end >= dest))
		*end-- = 0;
}
/*----
Pad a str from first null to len with spaces.
------*/
void unstrunc(char* str,int len)
{
	while(*str)
		{
		++str;
		--len;
		}
	while(len-- > 0)
		*str++ = ' ';
}

/*----
ASCII to integer for specified length.
------*/
int atoilen(char* src, int len)
{
   char buf[20];

   memcpy(buf,src,len);
   buf[len]=0;
   return(atoi(buf));
}
/*
**	History:
**	$Log: rcvs.c,v $
**	Revision 1.2  1996/09/17 23:45:45  gsl
**	drcs update
**	
**
**
*/
