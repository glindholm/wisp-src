			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
	setsymb		Set a VMS symbol to the given value.
			Returns 0 or the value of status.
*/

#ifdef VMS
#include <stdio.h>
#include <descrip.h>
#include <ssdef.h>
#include <libdef.h>

long setsymb(symbol,value,len,tabtype)
char	*symbol;								/* VMS Symbol name				*/
char	*value;									/* Value to load into symbol			*/
long	len;									/* Length of Value				*/
long	tabtype;								/* Symbol table to use.	(0 == default)		*/
{
	char symstr[256];
	char valstr[256];
	long status;
	$DESCRIPTOR(sym,symstr);
	$DESCRIPTOR(val,valstr);

	if (*symbol == '&') symbol++;						/* Skip over the '&'				*/

	memset(symstr,' ',256);								/* Fill symbol string with spaces.	*/
	symstr[255] = '\0';								/* Null terminate.			*/
	memcpy(symstr,symbol,strlen(symbol));						/* Copy in the symbol name		*/
	sym.dsc$w_length = strlen(symbol);						/* Set the length in the descriptor.	*/

	memset(valstr,' ',256);								/* Fill value string with spaces.	*/
	valstr[255] = '\0';								/* Null terminate.			*/
	memcpy(valstr,value,len);							/* Copy in the value to set it to.	*/
	val.dsc$w_length = len;								/* and its length			*/

	if (tabtype == 0)
	{
		status = lib$set_symbol(&sym,&val,0);					/* Set it's contents.			*/
	}
	else
	{
		status = lib$set_symbol(&sym,&val,&tabtype);				/* Set it's contents.			*/
	}

	if (status != SS$_NORMAL)
	{
		return(status);
	}
	return( 0 );
}

long getsymb(symbol,value,len)
char	*symbol;								/* VMS Symbol name				*/
char	*value;									/* Value to load into symbol			*/
long	*len;									/* Length of Value (optional)			*/
{
	char 	symstr[256];
	char 	valstr[256];
	long 	status;
	long 	tabtype;
	long 	length;
	$DESCRIPTOR(sym,symstr);
	$DESCRIPTOR(val,valstr);

	if (*symbol == '&') symbol++;							/* Skip over the '&'			*/

	memset(symstr,' ',256);								/* Fill symbol string with spaces.	*/
	symstr[255] = '\0';								/* Null terminate.			*/
	memcpy(symstr,symbol,strlen(symbol));						/* Copy in the symbol name		*/
	sym.dsc$w_length = strlen(symbol);						/* Set the length in the descriptor.	*/

	memset(valstr,' ',256);								/* Fill value string with spaces.	*/
	length = 0;	

	status = lib$get_symbol(&sym,&val,&length,&tabtype);				/* Set it's contents.			*/

	if (status != SS$_NORMAL)
	{
		return(status);
	}

	memcpy(value,valstr,length);							/* Copy out the value.			*/
	value[length] = '\0';								/* Null terminate the length		*/

	if (len)
		*len = length;								/* Set length if passed			*/

	return( 0 );
}
#endif


