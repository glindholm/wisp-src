static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/********************************************************************************************************************************
*																*
*	SORT.C  Emulation of WANG USERSUB:  SORT										*
*																*
********************************************************************************************************************************/

#if defined(unix) || defined(VMS) || defined(MSDOS_SORTX) || defined(WIN32)
/*
	For MSDOS this whole module gets included into sortx.c which defined MSDOS_SORTX.
*/

#include <stdlib.h>
#include <string.h>

#ifdef MSDOS
#include <search.h>
#endif

#include <varargs.h>                                                                    /* Allow variable number of arguments	*/
#include "idsistd.h"
#include "werrlog.h"
#include "wisplib.h"

#define		ROUTINE		60000


#ifdef VMS
#include <descrip.h>
#endif

static int4 keypos, keylen, elemsz;

static	void local_sort(char* in, char* out, int4 e_cnt, int4 e_len, int4 s_start, 
			int4 s_len, int s_type, int loc, int4 loc_sz);

static int ourcmp(const char** p1, const char** p2);
static int ourrcmp(const char** p1, const char** p2);
static int binncmp(		/* compare two areas of memory		*/
	const void *p1,		/* area one				*/
	const void *p2,		/* area two				*/
	int4 len);		/* number of bytes to compare		*/

void SORT(va_alist)

va_dcl
{
	va_list	the_args;								/* A pointer to traverse the stack.	*/
	int	arg_count;
	char	*input_array, *output_array, *sort_type, *locator_flag;			/* Pointers to passed arguments.	*/
	char	l_sort_type, l_locator_flag;
	int4	*element_count,  *element_length,  *sort_start,  *sort_length,  *locator_length;
	int4	l_element_count, l_element_length, l_sort_start, l_sort_length, l_locator_length;	/* Local copies - wswap	*/
	int4	i;


	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);						/* Say we are here.			*/

	/********************************************************
	*	Receive variable number of arguments		*
	********************************************************/
	va_start(the_args);								/* Set pointer to top of stack.		*/
	arg_count = va_count(the_args);							/* How many args are there ?		*/

	va_start(the_args);								/* Go back to the top of the stack.	*/

	input_array  = va_arg(the_args, char*);						/* Get address of the input array	*/
	output_array = input_array;							/* Default output array is input array	*/
	arg_count -= 1;									/* One less argument.			*/

	element_count = va_arg(the_args, int4*);					/* Get # of elements in input array	*/
	arg_count -= 1;									/* One less argument.			*/
	i = *element_count;
	wswap(&i);
	l_element_count = i;

	element_length = va_arg(the_args, int4*);					/* Length of each input element		*/
	arg_count -= 1;									/* One less argument.			*/
	i = *element_length;
	wswap(&i);
	l_element_length = i;

	l_sort_start = 1;								/* Default start sort key at position 1	*/
	l_sort_length = l_element_length;						/* Default key length is entire element	*/
	l_sort_type = 'A';								/* Default sort order is ascending	*/
	l_locator_flag = 'S';								/* Default is standard sort		*/

	/********************************************************
	*	Following are optional arguments 4 through 9	*
	********************************************************/

	if (arg_count > 0)
	{
		output_array = va_arg(the_args, char*);					/* Get address of the output array	*/
		arg_count -= 1;								/* One less argument.			*/
	}

	if (arg_count > 0)
	{
		sort_start = va_arg(the_args, int4*);					/* Starting position of sort field (1+)	*/
	 	arg_count -= 1;								/* One less argument.			*/
		i = *sort_start;
		wswap(&i);
		l_sort_start = i;
	}

	if (arg_count > 0)
	{
		sort_length = va_arg(the_args, int4*);					/* Length of the sort field		*/
	 	arg_count -= 1;								/* One less argument.			*/
		i = *sort_length;
		wswap(&i);
		l_sort_length = i;
	}

	if (arg_count > 0)
	{
		sort_type = va_arg(the_args, char*);					/* Ascending or Descending sort [A]	*/
		l_sort_type = *sort_type;
	 	arg_count -= 1;								/* One less argument.			*/
	}

	if (arg_count > 0)
	{
		locator_flag = va_arg(the_args, char*);					/* Flag for locator type sort		*/
		l_locator_flag = *locator_flag;
	 	arg_count -= 1;								/* One less argument.			*/
	}

	if (arg_count > 0)
	{
		locator_length = va_arg(the_args, int4*);				/* Desired size of each locator element	*/
	 	arg_count -= 1;								/* One less argument.			*/
		i = *locator_length;
		wswap(&i);
		l_locator_length = i;
	}

											/* standard c qsort()			*/
	if (l_locator_flag == 'L' || l_locator_flag == 'l')				/* gen err if Locator requested		*/
	{
		werrlog(ERRORCODE(2),0,0,0,0,0,0,0,0);
		return;
	}

	if (l_element_length > 255)							/* check their parms			*/
	{
		werrlog(ERRORCODE(4),l_element_length,0,0,0,0,0,0,0);
		return;
	}

	if (l_sort_length+(l_sort_start-1) > l_element_length || l_sort_start > l_element_length)
	{
		werrlog(ERRORCODE(6),l_sort_start,l_sort_length,0,0,0,0,0,0);
		return;

	}
											/* now call func to do the work 	*/
	local_sort(input_array, 								/* source array 			*/
		output_array, 								/* dest array 				*/
		l_element_count, 							/* number of elems in source array 	*/
		l_element_length,							/* length of each elem in source 	*/
		l_sort_start,								/* sort field pos in element 		*/
		l_sort_length,								/* sort field len in element 		*/
		(int)l_sort_type,							/* sort type: 'A' or 'D' 		*/
		(int)l_locator_flag,							/* standard sort or psuedo-pointer sort */
		l_locator_length); 							/* size (bytes) of locator 		*/

}

static	void local_sort(char* in, char* out, int4 e_cnt, int4 e_len, int4 s_start, 
			int4 s_len, int s_type, int loc, int4 loc_sz)
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

#endif /* unix || VMS || MSDOS_SORTX || WIN32 */
/*
**	History:
**	$Log: sort.c,v $
**	Revision 1.12  1997-03-12 13:12:54-05  gsl
**	changes to use WIN32
**
**	Revision 1.11  1996-08-19 18:32:57-04  gsl
**	drcs update
**
**
**
*/
