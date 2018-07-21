/*
******************************************************************************
**
** KCSI - King Computer Services Inc.
**
** 
******************************************************************************
*/

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
int   KCSI_lookup_strtkn_tkn(STRTKN *list,char *str);
char *KCSI_lookup_strtkn_str(STRTKN *list, int token);


/*
**	History:
**	$Log: strtkn.h,v $
**	Revision 1.5  2003/02/05 15:50:11  gsl
**	Fix copyright headers
**	
**	Revision 1.4  2002/10/24 15:48:30  gsl
**	Make globals unique
**	
**	Revision 1.3  1996/09/17 23:34:19  gsl
**	drcs update
**	
**
**
*/
