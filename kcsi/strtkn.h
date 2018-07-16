/* Copyright (c) 1988-1996 DevTech Migrations, All rights reserved. */
/* $Id:$ */
/*----
String token routines use an array of structures of type STR_TKN
Any such array must be terminated by an entry containing a token
value of zeroes.
STRTKN	list[]={
	{"HELLO",1},
	{"GOODBYE",2},
	{"",-1}		<--- closing entry
	};
------*/

typedef struct _str_tkn{
	char	*str;
	int	token;
	}STRTKN;

/* Some prototypes */
int lookup_strtkn_tkn();
char *lookup_strtkn_str();


/*
**	History:
**	$Log: strtkn.h,v $
**	Revision 1.3  1996/09/17 23:34:19  gsl
**	drcs update
**	
**
**
*/
