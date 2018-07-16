static char copyright[]="Copyright (c) 1988-1996 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
#include <stdio.h>
#include <ctype.h>
#include "strtkn.h"
#include "kcsifunc.h"

static char sccsid[]="@(#)strtkn.c	1.1 6/27/92";

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
**	Revision 1.2.2.1  2002/11/12 15:56:38  gsl
**	Sync with $HEAD Combined KCSI 4.0.00
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
