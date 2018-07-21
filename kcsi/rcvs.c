/*
******************************************************************************
**
** KCSI - King Computer Services Inc.
**
** 
******************************************************************************
*/


#include <ctype.h>
#include "kcsifunc.h"


/*----
Comparison and extraction routines.
------*/
/*----
Compare a field at a position in a record for len
with memory in m1.
------*/
int KCSI_fieldeq(char *record,char *base,int pos,int len,int type,int dec,char* m1)
{
	return(!(memcmp(record + pos,m1,len)));
}
/*----
Return a single character from a record at position pos.
------*/
int KCSI_charat(char *record,char *base,int pos,int len,int type,int dec)
{
	return(record[pos]);
}
/*----
ASCII at a field position in a record for length converted to
integer.
------*/
int KCSI_atoifield(char *record,char *base,int pos,int len,int type,int dec)
{
	return(kcsi_atoilen(record + pos,len));
}
void KCSI_strunc(char* dest)
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
void KCSI_unstrunc(char* str,int len)
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
int kcsi_atoilen(char* src, int len)
{
   char buf[20];

   memcpy(buf,src,len);
   buf[len]=0;
   return(atoi(buf));
}
/*
**	History:
**	$Log: rcvs.c,v $
**	Revision 1.4  2003/02/04 19:19:09  gsl
**	fix header
**	
**	Revision 1.3  2002/07/25 15:20:25  gsl
**	Globals
**	
**	Revision 1.2  1996/09/17 23:45:45  gsl
**	drcs update
**	
**
**
*/
