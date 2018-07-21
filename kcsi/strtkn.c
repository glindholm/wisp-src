/*
******************************************************************************
**
** KCSI - King Computer Services Inc.
**
** 
******************************************************************************
*/

#include <stdio.h>
#include <ctype.h>
#include "strtkn.h"
#include "kcsifunc.h"


/*----
Returns a value of -1 if the entry is not found.
------*/
int KCSI_lookup_strtkn_tkn(STRTKN *list,char *str)
{
	int len;

	len = strlen(str);

	while(list->token != -1)
		{
		if(!(strcmp(list->str,str)))
			break;
		++list;
		}
	return(list->token);
}
/*----
Returns a value of NULL if the entry is not found.
------*/
char *KCSI_lookup_strtkn_str(STRTKN *list, int token)
{

	while(list->token != -1)
		{
		if(token == list->token)
			break;
		++list;
		}
	if(list->token == -1)
		return(NULL);
	return(list->str);
}
/*
**	History:
**	$Log: strtkn.c,v $
**	Revision 1.4  2003/02/04 19:19:08  gsl
**	fix header
**	
**	Revision 1.3  2002/10/24 15:48:30  gsl
**	Make globals unique
**	
**	Revision 1.2  1996/09/17 23:45:53  gsl
**	drcs update
**	
**
**
*/
