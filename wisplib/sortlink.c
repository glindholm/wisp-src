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


/*
	SORTLINK	
	
	This VSSUB is another interface to the Wang VS SORT utility.
	It is used by some instead of SORTCALL.  It also allows access to 
	the MERGE and SELECTION features of the SORT utility.

	This implementation will extract the parameters and issue a call 
	the WISPSORT/SORTCALL.
	None of the MERGE or SELECTION criteria is supported and will cause an error.

	RETURN CODES
	51	Unable to decode arguments
	52	Invalid argument count
	54	Function not supported (only 'S' is supported)
	56	Option not supported

61502	%%SORTLINK-F-ARGCNT Invalid argument count arg_count=%ld
61504	%%SORTLINK-F-MERGE Function [%c] not supported
61506	%%SORTLINK-F-OPTION %s=%c not supported
61508	%%SORTLINK-F-OPTION %s=%ld not supported
61510	%%SORTLINK-F-ARGUMENTS Unable to decode arguments [%s] arg=%ld

*/

#include <stdio.h>
#include <string.h>
#include <ctype.h> 
#include <stdarg.h>

#include "idsistd.h"
#include "werrlog.h"
#include "wisplib.h"
#include "vssubs.h"

void SORTLINK(char* function, char* sort_options, ...)
{
	va_list the_args;								/* Declare variable to traverse list.	*/
	int4	total_arg_count;							/* Total number of arguments		*/
	int4	arg_count;								/* Number of arguments			*/
	char	*ptr;									/* Misc char pointer.			*/
	int4	return_code;								/* The return code			*/
	char	*return_ptr;								/* Ptr to return code			*/
	int4	tint4;									/* Temp int4				*/
	char	sortparms[116];								/* SORTCALL parameter			*/
	int4	keycount;								/* Number of sort keys			*/
	int4	i;									/* temp counter				*/
	char	buff[20];								/* temp buffer				*/
	int4	offset;									/* Offset into sortparms		*/
	int	dupinorder;								/* Duplicates are to be in order	*/

	char	filetype;								/* The file type (from SORTINFO)	*/
	int4	recsize;								/* The record size (from SORTINFO)	*/
	int4	*sortcode_ptr;								/* The sortcode pointer (from SORTINFO)	*/


	total_arg_count = WL_va_count();						/* How many args are there ?		*/
	WL_wtrace("SORTLINK","ENTRY","Entry into SORTLINK Function=[%c] Sort_options=[%c] args=%d",
		*function, *sort_options, total_arg_count);


	arg_count = total_arg_count;
	if (arg_count < 12 || arg_count > 500)
	{
		werrlog(WERRCODE(61502),arg_count,0,0,0,0,0,0,0);			/* Invalid number of parameters.	*/
		return_code = 52;
		goto return_label;
	}

	return_code = 0;
	memset(sortparms,' ',116);

	dupinorder = 0;									/* Duplicate are NOT in order		*/

	va_start(the_args, sort_options);

	/*
	**	Option Arguments Sequence
	*/

	/* ptr = va_arg(the_args, char*);  */						/* Get the FUNCTION			*/
	ptr = function;
	arg_count--;
	if ('S' != *ptr)
	{
		werrlog(WERRCODE(61504),*ptr,0,0,0,0,0,0,0);				/* Not Function=='S'			*/
		return_code = 54;
		goto return_label;
	}

	/* ptr = va_arg(the_args, char*); */						/* Get the SORT OPTIONS			*/
	ptr = sort_options;
	arg_count--;
	if ('3' == *ptr)
	{
		dupinorder = 1;								/* Stable sort				*/
	}
	else if ('0' != *ptr)
	{
		werrlog(WERRCODE(61506),"Sort option",*ptr,0,0,0,0,0,0);		/* Unsupported option			*/
		return_code = 56;
		goto return_label;
	}

	/*
	**	Input File Argument Sequence
	*/

	if (arg_count < 10)
	{
		werrlog(WERRCODE(61510),"INPUT seq",(int4)(total_arg_count-arg_count+1),0,0,0,0,0,0);
		return_code = 51;
		goto return_label;
	}

	ptr = va_arg(the_args, char*);							/* Get the FORMAT (optional)		*/
	arg_count--;
	if ('T' == *ptr)
	{
		werrlog(WERRCODE(61506),"Format option",*ptr,0,0,0,0,0,0);		/* Unsupported option			*/
		return_code = 56;
		goto return_label;
	}
	if ('D' == *ptr)
	{
		ptr = va_arg(the_args, char*);						/* Get INPUT FILE COUNT			*/
		arg_count--;
	}
	tint4 = WL_get_swap((int4*)ptr);
	if (1 != tint4)									/* Only one file supported		*/
	{
		werrlog(WERRCODE(61508),"Input file count",tint4,0,0,0,0,0,0);		/* Unsupported option			*/
		return_code = 56;
		goto return_label;
	}

	ptr = va_arg(the_args, char*);							/* Get INPUT FILE NAME			*/
	arg_count--;
	memcpy(&sortparms[0],ptr,8);

	ptr = va_arg(the_args, char*);							/* Get INPUT LIBRARY			*/
	arg_count--;
	memcpy(&sortparms[8],ptr,8);

	ptr = va_arg(the_args, char*);							/* Get INPUT VOLUME			*/
	arg_count--;
	memcpy(&sortparms[16],ptr,6);

	/*
	**	Selection Argument Sequence
	*/

	if (arg_count < 6)
	{
		werrlog(WERRCODE(61510),"SELECTION seq",(int4)(total_arg_count-arg_count+1),0,0,0,0,0,0);
		return_code = 51;
		goto return_label;
	}

	ptr = va_arg(the_args, char*);							/* Get SELECTION COUNT			*/
	arg_count--;
	tint4 = WL_get_swap((int4*)ptr);
	if (0 != tint4)									/* Only one file supported		*/
	{
		werrlog(WERRCODE(61508),"Selection count",tint4,0,0,0,0,0,0);		/* Unsupported option			*/
		return_code = 56;
		goto return_label;
	}

	/*
	**	SORT or MERGE Keys Argument Sequence
	*/

#define	SIZE_POSITION	4
#define	SIZE_LENGTH	3
#define	SIZE_TYPE	1
#define	SIZE_ORDER	1

	ptr = va_arg(the_args, char*);							/* Get KEY COUNT			*/
	arg_count--;
	keycount = WL_get_swap((int4*)ptr);
	if (keycount > 8 || keycount < 0)						/* Only one file supported		*/
	{
		werrlog(WERRCODE(61508),"Key count",keycount,0,0,0,0,0,0);		/* Unsupported option			*/
		return_code = 56;
		goto return_label;
	}

	offset = 44;

	for(i=0; i<keycount; i++)
	{
		if ( arg_count < ((keycount-i)*4 + 4) )
		{
			werrlog(WERRCODE(61510),"KEY seq",(int4)(total_arg_count-arg_count+1),0,0,0,0,0,0);
			return_code = 51;
			goto return_label;
		}

		ptr = va_arg(the_args, char*);						/* Get KEY POSITION			*/
		arg_count--;
		tint4 = WL_get_swap((int4*)ptr);
		sprintf(buff,"%04d",tint4);
		memcpy(&sortparms[offset],buff,SIZE_POSITION);
		offset += SIZE_POSITION;

		ptr = va_arg(the_args, char*);						/* Get KEY LENGTH			*/
		arg_count--;
		tint4 = WL_get_swap((int4*)ptr);
		sprintf(buff,"%03d",tint4);
		memcpy(&sortparms[offset],buff,SIZE_LENGTH);
		offset += SIZE_LENGTH;

		ptr = va_arg(the_args, char*);						/* Get KEY DATA TYPE			*/
		arg_count--;
		memcpy(&sortparms[offset],ptr,SIZE_TYPE);
		offset += SIZE_TYPE;

		ptr = va_arg(the_args, char*);						/* Get KEY SORT ORDER			*/
		arg_count--;
		memcpy(&sortparms[offset],ptr,SIZE_ORDER);
		offset += SIZE_ORDER;
	}

	/*
	**	Output File Argument Sequence
	*/

	if (arg_count < 4)
	{
		werrlog(WERRCODE(61510),"OUTPUT seq",(int4)(total_arg_count-arg_count+1),0,0,0,0,0,0);
		return_code = 51;
		goto return_label;
	}

	ptr = va_arg(the_args, char*);							/* Get OUTPUT FILE NAME			*/
	arg_count--;
	memcpy(&sortparms[22],ptr,8);

	ptr = va_arg(the_args, char*);							/* Get OUTPUT LIBRARY			*/
	arg_count--;
	memcpy(&sortparms[30],ptr,8);

	ptr = va_arg(the_args, char*);							/* Get OUTPUT VOLUME			*/
	arg_count--;
	memcpy(&sortparms[38],ptr,6);

	if (arg_count == 3)
	{
		ptr = va_arg(the_args, char*);						/* Get FILE FORMAT			*/
		arg_count--;
		if ('D' != *ptr)
		{
			werrlog(WERRCODE(61506),"Output format",*ptr,0,0,0,0,0,0);	/* Unsupported option			*/
			return_code = 56;
			goto return_label;
		}
	}

	if (arg_count == 2)
	{
		ptr = va_arg(the_args, char*);						/* Get TAPE FILE SEQUENCE		*/
		arg_count--;
	}

	/*
	**	Return Code Argument Sequence
	*/

	if (arg_count != 1)
	{
		werrlog(WERRCODE(61510),"RETCODE",(int4)(total_arg_count-arg_count+1),0,0,0,0,0,0);
		return_code = 51;
		goto return_label;
	}

return_label:

	while( arg_count )
	{
		ptr = va_arg(the_args, char*);						/* Get RETURN CODE			*/
		arg_count--;
	}

	return_ptr = ptr;

	if ( return_code > 0 )
	{
		WL_put_swap(return_ptr, return_code);
		return;
	}

	WL_getsortinfo(&filetype, &recsize, &sortcode_ptr);

	WL_wangsort(sortparms,&filetype,&recsize,dupinorder,sortcode_ptr,(int4 *)return_ptr);
	return;
}

static char	sort_filetype = ' ';						/* The sort filetype 	(SORTLINK,SORTCALL)	*/
static int4	sort_recsize = 0;						/* The sort record size	(SORTLINK,SORTCALL)	*/
static int4	*sort_sortcode_ptr;						/* The sort sortcode	(SORTLINK,SORTCALL)	*/
static int4	sort_dummy;							/* Dummy sortcode				*/

/*
	SORTINFO	This routine is used to pass extra info to the routines SORTLINK and SORTCALL.
			If not called they will default to filetype="I" (INDEXED).
*/
void SORTINFO(char* filetype, int4* recsize, int4* sortcode)
{
	sort_filetype = *filetype;

	if (recsize) sort_recsize = *recsize;
	else	     sort_recsize = 0;

	if (sortcode) sort_sortcode_ptr = sortcode;
	else	      sort_sortcode_ptr = &sort_dummy;
}

/*
	WL_getsortinfo	This routine is call by SORTCALL and SORTLINK to get the extra info supplied by SORTINFO.
			It can only be called once, then it will reset the variable to an uninitialized state.
*/
void WL_getsortinfo(char* filetype, int4* recsize, int4** sortcode_ptr)
{
	if (' ' == sort_filetype)
	{
		*filetype = 'I';
		*recsize = 0;
		*sortcode_ptr = &sort_dummy;
	}
	else 
	{
		*filetype = sort_filetype;
		*recsize = sort_recsize;
		*sortcode_ptr = sort_sortcode_ptr;
	}

	sort_filetype = ' ';
	sort_recsize = 0;
	sort_sortcode_ptr = &sort_dummy;
}
/*
**	History:
**	$Log: sortlink.c,v $
**	Revision 1.20  2003/02/21 19:18:32  gsl
**	Switch SORTLINK to stdarg.h
**	
**	Revision 1.19  2003/02/17 22:07:17  gsl
**	move VSSUB prototypes to vssubs.h
**	
**	Revision 1.18  2003/02/04 16:30:02  gsl
**	Fix -Wall warnings
**	
**	Revision 1.17  2003/01/31 18:54:37  gsl
**	Fix copyright header
**	
**	Revision 1.16  2002/12/10 20:54:11  gsl
**	use WERRCODE()
**	
**	Revision 1.15  2002/12/09 21:09:32  gsl
**	Use WL_wtrace(ENTRY)
**	
**	Revision 1.14  2002/10/18 19:14:09  gsl
**	Cleanup
**	
**	Revision 1.13  2002/07/16 16:24:52  gsl
**	Globals
**	
**	Revision 1.12  2002/07/12 19:10:17  gsl
**	Global unique WL_ changes
**	
**	Revision 1.11  2002/07/12 17:01:01  gsl
**	Make WL_ global unique changes
**	
**	Revision 1.10  2002/07/11 20:29:14  gsl
**	Fix WL_ globals
**	
**	Revision 1.9  2002/07/10 21:05:25  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.8  1996/08/19 22:32:58  gsl
**	drcs update
**	
**
**
*/
