			/************************************************************************/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*			Copyright (c) 1988, 1989, 1990			*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/


#define		ROUTINE		35000

#include <stdio.h>
#include <varargs.h>

#ifdef unix
#include <fcntl.h>
#include <errno.h>
#endif

#include "idsistd.h"
#include "movebin.h"
#include "werrlog.h"
#include "wdefines.h"

int4	bsrch();

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

int4	bsrch(table,n_elem,e_len,s_item,s_len)
char	*table,*s_item;
int4	n_elem,e_len,s_len;
{
	int	order;
	int4	top,bottom,middle;

	char foo[15],bar[15],baz[15];
	
#define IND(_ind) (table+(_ind)*e_len)

	bottom = 0;
	top    = n_elem-1;
	middle = (top+bottom)/2;

	if (strncmp(IND(bottom),IND(top),(int)e_len)<0) order = ASCENDING;
	else order = DESCENDING;

	while (order*strncmp(s_item,IND(middle),(int)s_len) && bottom<=top)
	{
		if (order*strncmp(s_item,IND(middle),(int)s_len)<0) top=middle-1;
		if (order*strncmp(s_item,IND(middle),(int)s_len)>0) bottom=middle+1;
		middle = (top+bottom)/2;
	}
	
	if (strncmp(s_item,IND(middle),(int)s_len)==0) return middle+1;
	else return 0;
}

