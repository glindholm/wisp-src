/*
******************************************************************************
**
** KCSI - King Computer Services Inc.
**
** 
******************************************************************************
*/


/*----
Determines the external (display length) of a field based on
passed parameters, and generates a cobol pic for it.

Character int len = ext-len pic X(x_len)

Binary ext = int_len * 2 pic = x(x_len)
	or
with bin edit
	ext = int_len * 2 + 2 + 1 extra if internal
		len = 4. thus

bin int len = 1 is 999-
	      2 is 99999-
              3 is 9999999-
              4 is 9999999999-

packed = int len * 2 - 1 + 1 for dec and 1 for sign
zoned = int len + 1 for dec and 1 for sign
unsigned = int len + 1 for dec and 1 for sign


returns x_len;
------*/

#include "kcsifunc.h"

/*----
Not used

get_ext_len(type,len,dec,bin)
int type,len,dec,bin;
{
	char junk[101];

	return(KCSI_get_pic_len(junk,type,len,dec,bin));
}
------*/

int KCSI_get_pic_len(char* dest,int type,int len,int dec,int bin)
{
	int x_len;

	dest[0] = '\0';

	x_len = 0;
	switch(type)
		{
		case 'C':
			x_len = len;
			sprintf(dest,"X(%d)",len);
			break;
		case 'P':
			x_len = len - 1;
		case 'Z':
		case 'U':
			x_len += len;
			if(x_len - dec)
				sprintf(dest,"9(%d)",x_len - dec);
			if(dec)
				{
				strcat(dest,".");
				++x_len;
				dest += strlen(dest);
				sprintf(dest,"9(%d)",dec);
				}
			if(type == 'U')
				break;
/* add sign for zoned and packed*/
			++x_len;
			strcat(dest,"-");
			break;
		case 'B':
			x_len = len * 2;
			if(bin)
				{
				x_len += 2;
				if(len == 4)
					x_len += 1;
				sprintf(dest,"9(%d)-",x_len - 1);
				}
			else
				{
				sprintf(dest,"X(%d)",x_len);
				}
			break;
		}
	return(x_len);
}

/*
**	History:
**	$Log: piclen.c,v $
**	Revision 1.5  2003/02/04 19:19:09  gsl
**	fix header
**	
**	Revision 1.4  2002/10/24 14:20:37  gsl
**	Make globals unique
**	
**	Revision 1.3  1998/04/08 15:44:53  gsl
**	Init the dest to null.
**	
**	Revision 1.2  1996-09-17 19:45:43-04  gsl
**	drcs update
**
**
**
*/
