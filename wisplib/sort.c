/*
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
** $Id:$
**
** NOTICE:
** Confidential, unpublished property of NeoMedia Technologies, Inc.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of NeoMedia 
** Technologies, Inc. is strictly prohibited.
** 
** CVS
** $Source:$
** $Author: gsl $
** $Date:$
** $Revision:$
*/


/********************************************************************************************************************************
*																*
*	SORT.C  Emulation of WANG USERSUB:  SORT										*
*																*
********************************************************************************************************************************/


#include <stdlib.h>
#include <string.h>


#include <stdarg.h>                                                                    /* Allow variable number of arguments	*/
#include "idsistd.h"
#include "werrlog.h"
#include "wisplib.h"

static int4 keypos, keylen, elemsz;

static	void local_sort(char* in, char* out, int4 e_cnt, int4 e_len, int4 s_start, 
			int4 s_len, char s_type);

static int ourcmp(const char** p1, const char** p2);
static int ourrcmp(const char** p1, const char** p2);
static int binncmp(		/* compare two areas of memory		*/
	const void *p1,		/* area one				*/
	const void *p2,		/* area two				*/
	int4 len);		/* number of bytes to compare		*/

/*
	SORT()

	Arg4 thur Arg9 are optional but if one is included all previous args
	must be included.

	ARG1	Input		Alpha(var)
	ARG2	Elements	Int(4)
	ARG3	Element_length	Int(4)
	ARG4	Output		Alpha(var)	Optional
	ARG5	Start		Int(4)		Optional
	ARG6	Sort_length	Int(4)		Optional
	ARG7	Sort_type	Alpha(1)	Optional
	ARG8	Locator_flag	Alpha(1)	Optional
	ARG9	Locator_length	Int(4)		Optional
*/
void SORT(char* input_array, int4* element_count, int4* element_length, ...)
{
	va_list	the_args;
	int	arg_count;
	char	*output_array, *sort_type, *locator_flag;
	char	l_sort_type, l_locator_flag;
	int4	*sort_start,  *sort_length,  *locator_length;
	int4	l_element_count, l_element_length, l_sort_start, l_sort_length, l_locator_length;

	va_start(the_args, element_length);
	arg_count = WL_va_count();

	WL_wtrace("SORT","ENTRY","Entry into SORT args=%d", arg_count);


	/* ARG1	Input		Alpha(var)	*/

	/* ARG2	Elements	Int(4)  */
	l_element_count = WL_get_swap(element_count);

	/* ARG3	Element_length	Int(4)	*/
	l_element_length = WL_get_swap(element_length);


	/* ARG4	Output		Alpha(var)	Optional */
	if (arg_count >= 4)
	{
		output_array = va_arg(the_args, char*);					/* Get address of the output array	*/
	}
	else
	{
		output_array = input_array;						/* Default output array is input array	*/
	}

	/* ARG5	Start		Int(4)		Optional */
	if (arg_count >= 5)
	{
		sort_start = va_arg(the_args, int4*);					/* Starting position of sort field (1+)	*/
		l_sort_start = WL_get_swap(sort_start);
	}
	else
	{
		l_sort_start = 1;							/* Default start sort key at position 1	*/
	}

	/* ARG6	Sort_length	Int(4)		Optional */
	if (arg_count >= 6)
	{
		sort_length = va_arg(the_args, int4*);					/* Length of the sort field		*/
		l_sort_length = WL_get_swap(sort_length);
	}
	else
	{
		l_sort_length = l_element_length;					/* Default key length is entire element	*/
	}

	/* ARG7	Sort_type	Alpha(1)	Optional */
	if (arg_count >= 7)
	{
		sort_type = va_arg(the_args, char*);					/* Ascending or Descending sort [A]	*/
		l_sort_type = *sort_type;
	}
	else
	{
		l_sort_type = 'A';							/* Default sort order is ascending	*/
	}

	/* ARG8	Locator_flag	Alpha(1)	Optional */
	if (arg_count >= 8)
	{
		locator_flag = va_arg(the_args, char*);					/* Flag for locator type sort		*/
		l_locator_flag = *locator_flag;
	}
	else
	{
		l_locator_flag = 'S';							/* Default is standard sort		*/
	}

	/* ARG9	Locator_length	Int(4)		Optional */
	if (arg_count == 9)
	{
		locator_length = va_arg(the_args, int4*);				/* Desired size of each locator element	*/
		l_locator_length = WL_get_swap(locator_length);
	}
	else
	{
		l_locator_length = 0;
	}
	va_end(the_args);
											/* standard c qsort()			*/
	if (l_locator_flag == 'L' || l_locator_flag == 'l')				/* gen err if Locator requested		*/
	{
		werrlog(WERRCODE(60002),0,0,0,0,0,0,0,0);
		return;
	}

	if (l_element_length > 255)							/* check their parms			*/
	{
		werrlog(WERRCODE(60004),l_element_length,0,0,0,0,0,0,0);
		return;
	}

	if (l_sort_length+(l_sort_start-1) > l_element_length || l_sort_start > l_element_length)
	{
		werrlog(WERRCODE(60006),l_sort_start,l_sort_length,0,0,0,0,0,0);
		return;

	}

	WL_wtrace("SORT","ARGS","Elements=[%d] Element Length=[%d], Start=[%d] Sort Length=[%d] Sort type=[%c]",
		l_element_count, l_element_length, l_sort_start, l_sort_length, l_sort_type);

											/* now call func to do the work 	*/
	local_sort(input_array, 							/* source array 			*/
		output_array, 								/* dest array 				*/
		l_element_count, 							/* number of elems in source array 	*/
		l_element_length,							/* length of each elem in source 	*/
		l_sort_start,								/* sort field pos in element 		*/
		l_sort_length,								/* sort field len in element 		*/
		l_sort_type);								/* sort type: 'A' or 'D' 		*/

}

static	void local_sort(char* in, char* out, int4 e_cnt, int4 e_len, int4 s_start, 
			int4 s_len, char s_type)
{
	int (*comp)(const void*, const void*);						/* pointer to compare fn for qsort()	*/
	char *buf, **p, **ptrs;
	int i;

	elemsz = e_len;									/* globals that compare routines 	*/
	keypos = s_start-1;								/* must know about 			*/
	keylen = s_len;
	
	if (s_type=='D') comp = (int (*)(const void*, const void*))ourrcmp;		/* reverse compare 			*/
	else comp = (int (*)(const void*, const void*))ourcmp;				/* normal compare 			*/

	ptrs = (char **)calloc((int)e_cnt,sizeof(char *));				/* grab space for pointer list 		*/
	for (buf=in, p=ptrs, i=e_cnt; i; --i, buf += e_len, ++p)			/* build pointer list for qsort 	*/
		*p = buf;

	qsort((char *)ptrs, (int)e_cnt, sizeof(char *), comp);				/* sort the array			*/

	if (out==in)
		buf = (char *)calloc((int)(e_cnt+1), (int)e_len);			/* grab a temp buffer 			*/
	else 
		buf = out;								/* point to buffer they gave us 	*/

	memset(buf,(int)' ',(int)(e_cnt*e_len));					/* blank the temp buf 			*/
	for (p=ptrs, i=0; i<e_cnt; ++i, ++p)
	{
		memcpy(buf+i*e_len,*p,(int)e_len);					/* copy sorted data into temp buf 	*/
	}
	if (out==in)									/* sort in place ?			*/
		memcpy(out,buf,(int)(e_cnt*e_len));					/* copy temp buf back to orig 		*/

	if (out==in) free(buf);	    							/* zed the temp buf 			*/
	free(ptrs);									/* and pointer list 			*/
}

static int ourcmp(const char** p1, const char** p2)		     			/* normal compare			*/
{											/* supplied that way by qsort()		*/
	return binncmp(*p1+keypos,*p2+keypos,keylen);					/* call binncmp() to do binary compare	*/
}											/* keylen is global set by _sort	*/

static int ourrcmp(const char** p1, const char** p2)					/* reverse compare			*/
{
	return binncmp(*p2+keypos,*p1+keypos,keylen);
}

static int binncmp(		/* compare two areas of memory		*/
	const void *p1,		/* area one				*/
	const void *p2,		/* area two				*/
	int4 len)		/* number of bytes to compare		*/
{
	const unsigned char *s1 = (const unsigned char *)p1;
	const unsigned char *s2 = (const unsigned char *)p2;

	while (len)								
	{
		if (*s1 != *s2) return *s1 - *s2;					/* compare byte by byte			*/
		++s1; ++s2; --len;							/* decrement and continue		*/
	}
	return 0;									/* areas are the same			*/
}

/*
**	History:
**	$Log: sort.c,v $
**	Revision 1.21  2003/01/31 18:54:37  gsl
**	Fix copyright header
**	
**	Revision 1.20  2003/01/20 18:15:45  gsl
**	Changed to use stdarg.h
**	
**	Revision 1.19  2002/12/10 20:54:11  gsl
**	use WERRCODE()
**	
**	Revision 1.18  2002/12/09 21:09:31  gsl
**	Use WL_wtrace(ENTRY)
**	
**	Revision 1.17  2002/07/16 16:24:52  gsl
**	Globals
**	
**	Revision 1.16  2002/07/12 17:01:01  gsl
**	Make WL_ global unique changes
**	
**	Revision 1.15  2002/07/11 20:29:14  gsl
**	Fix WL_ globals
**	
**	Revision 1.14  2002/06/21 03:10:41  gsl
**	Remove VMS & MSDOS
**	
**	Revision 1.13  1998/07/10 15:02:58  gsl
**	Initialized the l_locator_length variable
**	
**	Revision 1.12  1997-03-12 13:12:54-05  gsl
**	changes to use WIN32
**
**	Revision 1.11  1996-08-19 18:32:57-04  gsl
**	drcs update
**
**
**
*/
