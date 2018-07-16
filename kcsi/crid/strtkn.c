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
lookup_strtkn_tkn(list,str)
STRTKN *list;
char *str;
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
char *lookup_strtkn_str(list,token)
STRTKN *list;
int token;
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
**	Revision 1.2  1996-09-17 19:45:53-04  gsl
**	drcs update
**
**
**
*/
