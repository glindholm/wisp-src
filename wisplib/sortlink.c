			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
	SORTLINK	This VSSUB is another interface to the Wang VS SORT utility.
			It is used by some instead of SORTCALL.  It also allows access to the MERGE and SELECTION
			features of the SORT utility.

			This implementation will extract the parameters and issue a call the WISPSORT/SORTCALL.
			None of the MERGE or SELECTION criteria is supported and will cause an error.

			RETURN CODES
			51	Unable to decode arguments
			52	Invalid argument count
			54	Function not supported (only 'S' is supported)
			56	Option not supported

61501	%%SORTLINK-I-ENTRY Entry into SORTLINK
61502	%%SORTLINK-F-ARGCNT Invalid argument count arg_count=%ld
61504	%%SORTLINK-F-MERGE Function [%c] not supported
61506	%%SORTLINK-F-OPTION %s=%c not supported
61508	%%SORTLINK-F-OPTION %s=%ld not supported
61510	%%SORTLINK-F-ARGUMENTS Unable to decode arguments [%s] arg=%ld

*/

#include <ctype.h> 
#include <varargs.h>

#include "idsistd.h"
#include "movebin.h"
#include "werrlog.h"
#include "cobrun.h"

void SORTLINK(va_alist)						                        /* Function uses variable arguments.	*/
va_dcl											/* Parameter declaration.     		*/
{
#define		ROUTINE		61500
	va_list the_args;								/* Declare variable to traverse list.	*/
	int4	total_arg_count;							/* Total number of arguments		*/
	int4	arg_count;								/* Number of arguments			*/
	char	*ptr;									/* Misc char pointer.			*/
	int4	return_code;								/* The return code			*/
	char	*return_ptr;								/* Ptr to return code			*/
	int4	tlong;									/* Temp int4				*/
	char	sortparms[116];								/* SORTCALL parameter			*/
	int4	keycount;								/* Number of sort keys			*/
	int4	i;									/* temp counter				*/
	char	buff[20];								/* temp buffer				*/
	int4	offset;									/* Offset into sortparms		*/
	int	dupinorder;								/* Duplicates are to be in order	*/

	char	filetype;								/* The file type (from SORTINFO)	*/
	int4	recsize;								/* The record size (from SORTINFO)	*/
	int4	*sortcode_ptr;								/* The sortcode pointer (from SORTINFO)	*/


	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);						/* Say we are here.			*/

	return_code = 0;
	memset(sortparms,' ',116);

	dupinorder = 0;									/* Duplicate are NOT in order		*/

	va_start(the_args);								/* Setup pointer to start of list.	*/
	total_arg_count = va_count(the_args);						/* How many args are there ?		*/
	arg_count = total_arg_count;
	if (arg_count < 12 || arg_count > 500)
	{
		werrlog(ERRORCODE(2),arg_count,0,0,0,0,0,0,0);				/* Invalid number of parameters.	*/
		return_code = 52;
		goto return_label;
	}

	va_start(the_args);								/* Reset the pointer.			*/

	/*
	**	Option Arguments Sequence
	*/

	ptr = va_arg(the_args, char*);  						/* Get the FUNCTION			*/
	arg_count--;
	if ('S' != *ptr)
	{
		werrlog(ERRORCODE(4),*ptr,0,0,0,0,0,0,0);				/* Not Function=='S'			*/
		return_code = 54;
		goto return_label;
	}

	ptr = va_arg(the_args, char*);							/* Get the SORT OPTIONS			*/
	arg_count--;
	if ('3' == *ptr)
	{
		dupinorder = 1;								/* Stable sort				*/
	}
	else if ('0' != *ptr)
	{
		werrlog(ERRORCODE(6),"Sort option",*ptr,0,0,0,0,0,0);			/* Unsupported option			*/
		return_code = 56;
		goto return_label;
	}

	/*
	**	Input File Argument Sequence
	*/

	if (arg_count < 10)
	{
		werrlog(ERRORCODE(10),"INPUT seq",(int4)(total_arg_count-arg_count+1),0,0,0,0,0,0);
		return_code = 51;
		goto return_label;
	}

	ptr = va_arg(the_args, char*);							/* Get the FORMAT (optional)		*/
	arg_count--;
	if ('T' == *ptr)
	{
		werrlog(ERRORCODE(6),"Format option",*ptr,0,0,0,0,0,0);			/* Unsupported option			*/
		return_code = 56;
		goto return_label;
	}
	if ('D' == *ptr)
	{
		ptr = va_arg(the_args, char*);						/* Get INPUT FILE COUNT			*/
		arg_count--;
	}
	GETBIN(&tlong,ptr,4);
	wswap(&tlong);
	if (1 != tlong)									/* Only one file supported		*/
	{
		werrlog(ERRORCODE(8),"Input file count",tlong,0,0,0,0,0,0);		/* Unsupported option			*/
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
		werrlog(ERRORCODE(10),"SELECTION seq",(int4)(total_arg_count-arg_count+1),0,0,0,0,0,0);
		return_code = 51;
		goto return_label;
	}

	ptr = va_arg(the_args, char*);							/* Get SELECTION COUNT			*/
	arg_count--;
	GETBIN(&tlong,ptr,4);
	wswap(&tlong);
	if (0 != tlong)									/* Only one file supported		*/
	{
		werrlog(ERRORCODE(8),"Selection count",tlong,0,0,0,0,0,0);		/* Unsupported option			*/
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
	GETBIN(&keycount,ptr,4);
	wswap(&keycount);
	if (keycount > 8 || keycount < 0)						/* Only one file supported		*/
	{
		werrlog(ERRORCODE(8),"Key count",keycount,0,0,0,0,0,0);			/* Unsupported option			*/
		return_code = 56;
		goto return_label;
	}

	offset = 44;

	for(i=0; i<keycount; i++)
	{
		if ( arg_count < ((keycount-i)*4 + 4) )
		{
			werrlog(ERRORCODE(10),"KEY seq",(int4)(total_arg_count-arg_count+1),0,0,0,0,0,0);
			return_code = 51;
			goto return_label;
		}

		ptr = va_arg(the_args, char*);						/* Get KEY POSITION			*/
		arg_count--;
		GETBIN(&tlong,ptr,4);
		wswap(&tlong);
		sprintf(buff,"%04ld",tlong);
		memcpy(&sortparms[offset],buff,SIZE_POSITION);
		offset += SIZE_POSITION;

		ptr = va_arg(the_args, char*);						/* Get KEY LENGTH			*/
		arg_count--;
		GETBIN(&tlong,ptr,4);
		wswap(&tlong);
		sprintf(buff,"%03ld",tlong);
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
		werrlog(ERRORCODE(10),"OUTPUT seq",(int4)(total_arg_count-arg_count+1),0,0,0,0,0,0);
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
			werrlog(ERRORCODE(6),"Output format",*ptr,0,0,0,0,0,0);		/* Unsupported option			*/
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
		werrlog(ERRORCODE(10),"RETCODE",(int4)(total_arg_count-arg_count+1),0,0,0,0,0,0);
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
		wswap(&return_code);
		PUTBIN(return_ptr,&return_code,4);
		return;
	}

	getsortinfo(&filetype, &recsize, &sortcode_ptr);

	wangsort(sortparms,&filetype,&recsize,dupinorder,sortcode_ptr,(int4 *)return_ptr);
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
void SORTINFO(filetype,recsize,sortcode)
char	*filetype;
int4	*recsize;
int4	*sortcode;
{
	sort_filetype = *filetype;

	if (recsize) sort_recsize = *recsize;
	else	     sort_recsize = 0;

	if (sortcode) sort_sortcode_ptr = sortcode;
	else	      sort_sortcode_ptr = &sort_dummy;
}

/*
	getsortinfo	This routine is call by SORTCALL and SORTLINK to get the extra info supplied by SORTINFO.
			It can only be called once, then it will reset the variable to an uninitialized state.
*/
getsortinfo(filetype,recsize,sortcode_ptr)
char	*filetype;
int4	*recsize;
int4	**sortcode_ptr;
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
