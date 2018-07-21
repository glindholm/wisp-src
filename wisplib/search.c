/*
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
** NOTICE:
** Confidential, unpublished property of Shell Stream Software LLC.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of Shell Stream Software LLC
** is strictly prohibited.
** 
*/



#define		ROUTINE		35000

#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <errno.h>

#ifdef unix
#include <fcntl.h>
#endif

#include "idsistd.h"
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
**	arg1	table		Alpha(var)	The table to search (Must be sorted A or D)
**	arg2	table_size	Int(4)		The number of items in the table
**	arg3	item_length	Int(4)		The length of each table item
**	arg4	search_item	Alpha(var)	Value to search for.
**	arg5	search_len	Int(4)		Lenght of search_item (Optional)
**	arg6	ret_code	Int(4)		Return code
**					0 = not found
**					n = position in table
**
*/
void SEARCH(char* table, 
	    int4* table_size_arg, 
	    int4* item_length_arg,
	    char* search_item,
	    ...)
{
	va_list arg_list;

	int	arg_count;
	int4	*long_item;
	int4	table_size,item_length,search_len;
	int4	*ret_code,local_ret;

	arg_count = WL_va_count();
	if (arg_count < 5 || arg_count > 6)
	{
		WL_werrlog_error(WERRCODE(57502), "SEARCH", "ARGCNT", "Invalid number of arguments [cnt=%d]", arg_count);
		return;
	}

	table_size = WL_get_swap(table_size_arg);		/* get swap  table size (number of elems) */
	item_length = WL_get_swap(item_length_arg);		/* get swap item length */

	va_start(arg_list, search_item);
	/* ARG5 - serach_len */
	if(arg_count > 5)
	{
		long_item=va_arg(arg_list,int4*);
		search_len = WL_get_swap(long_item);
	}
	else
	{
		search_len=item_length;
	}

	/* ARG6 - ret_code */
	ret_code=va_arg(arg_list,int4*);
	va_end(arg_list);
	
	local_ret=bsrch(table,table_size,item_length,search_item,search_len);
	WL_put_swap(ret_code, local_ret);
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
**	Revision 1.15  2003/01/31 18:54:38  gsl
**	Fix copyright header
**	
**	Revision 1.14  2003/01/17 18:49:50  gsl
**	Switch SEARCH to use stdarg.h
**	
**	Revision 1.13  2002/07/16 16:24:52  gsl
**	Globals
**	
**	Revision 1.12  2002/07/12 17:01:00  gsl
**	Make WL_ global unique changes
**	
**	Revision 1.11  2002/07/11 20:29:13  gsl
**	Fix WL_ globals
**	
**	Revision 1.10  1996/08/19 22:32:53  gsl
**	drcs update
**	
**
**
*/
