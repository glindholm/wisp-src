static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988, 1989, 1990, 1991, 1992, 1993		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/


#define		ROUTINE		35000

#include <stdio.h>
#include <varargs.h>
#include <errno.h>

#ifdef unix
#include <fcntl.h>
#endif

#include "idsistd.h"
#include "movebin.h"
#include "werrlog.h"
#include "wdefines.h"
#include "wisplib.h"


static int4 bsrch(char* table, int4 n_elem, int4 e_len, char* s_item, int4 s_len);

/*
**	Routine:	SEARCH()
**
**	Function:	The VSSUB SEARCH 
**
**	Description:	Searches a table for a matching string.
**			The table must sorted as it does a binary search.
**
**	Arguments:
**	table		Alpha(var)	The table to search (Must be sorted A or D)
**	table_size	Int(4)		The number of items in the table
**	item_length	Int(4)		The length of each table item
**	search_item	Alpha(var)	Value to search for.
**	search_len	Int(4)		Lenght of search_item (Optional)
**	ret_code	Int(4)		Return code
**					0 = not found
**					n = position in table
**
**	Globals:	None
**
**	Return:		None
**
**	Warnings:	None
**
**	History:	
**	12/13/93	Fix to allow binary data in the table. GSL
**
*/
void SEARCH( va_alist )
va_dcl
{
	va_list arg_list;

	int	arg_count;
	int4	*long_item;
	char	*table,*search_item;
	int4	table_size,item_length,search_len;
	int4	*ret_code,local_ret;

	va_start(arg_list);
	arg_count = va_count(arg_list);
	va_start(arg_list);

	table=va_arg(arg_list,char*);                                                   /* get table address */

	long_item=va_arg(arg_list,int4*);
	GETBIN(&table_size,long_item,4);						/* get table size (number of elems) */
	wswap(&table_size);

	long_item=va_arg(arg_list,int4*);
	GETBIN(&item_length,long_item,4);						/* get item length */
	wswap(&item_length);
	
	search_item=va_arg(arg_list,char*);						/* get search item */

	if(arg_count==6)
	{
		long_item=va_arg(arg_list,int4*);
		GETBIN(&search_len,long_item,4);						/* get search length */
		wswap(&search_len);
	}
	else
	{
		search_len=item_length;
	}

	ret_code=va_arg(arg_list,int4*);							/* get pointer to return code */
	
	local_ret=bsrch(table,table_size,item_length,search_item,search_len);
	PUTBIN(ret_code,&local_ret,4);
	wswap(ret_code);
}

#define ASCENDING 1
#define DESCENDING -1

static int4 bsrch(char* table, int4 n_elem, int4 e_len, char* s_item, int4 s_len)
{
	int	order;
	int4	top,bottom,middle;

#define IND(_ind) (table+(_ind)*e_len)

	bottom = 0;
	top    = n_elem-1;
	middle = (top+bottom)/2;

	if (memcmp(IND(bottom),IND(top),(int)e_len)<0) order = ASCENDING;
	else order = DESCENDING;

	while (order*memcmp(s_item,IND(middle),(int)s_len) && bottom<=top)
	{
		if (order*memcmp(s_item,IND(middle),(int)s_len)<0) top=middle-1;
		if (order*memcmp(s_item,IND(middle),(int)s_len)>0) bottom=middle+1;
		middle = (top+bottom)/2;
	}
	
	if (memcmp(s_item,IND(middle),(int)s_len)==0) return middle+1;
	else return 0;
}

/*
**	History:
**	$Log: search.c,v $
**	Revision 1.10  1996-08-19 18:32:53-04  gsl
**	drcs update
**
**
**
*/
