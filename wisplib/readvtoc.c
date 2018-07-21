/*
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
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
*/


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "idsistd.h"
#include "werrlog.h"
#include "wfname.h"
#include "wisplib.h"
#include "vssubs.h"
#include "wmalloc.h"



static int4 do_vtoc_find( char *lib, char *vol, int4 *start, int4 *count, char *rcvr, int4 *total);

void READVTOC(char *option, char *lib, char *vol, int4 *start, int4 *count, char *rcvr, int4 *rtrn_code, int4 *total)
{

	WL_wtrace("READVTOC","ENTRY","Entry into READVTOC(%c, %8.8s, %6.6s, ...)",
		*option,lib,vol);

	switch( toupper(*option) )
	{
		case 'F':								/* The "F" option is like a FIND call.	*/
		{
			int4	x_count, x_total, x_rc;

			do_vtoc_find( lib, vol, start, count, rcvr, total );

			x_count = WL_get_swap(count);
			x_total = WL_get_swap(total);

			x_rc =  (x_count > 0) ? 0 : 24;
			WL_put_swap(rtrn_code, x_rc);

			wtrace("READVTOC","RETURN","Count=%ld Total=%ld RC=%ld.",
						(long)x_count, (long)x_total, (long)x_rc);
			break;
		}
		case 'L':
		case 'V':
			werrlog(WERRCODE(52002),*option,0,0,0,0,0,0,0);
			break;
		case 'G':
		case 'X':
		case '#':
		case 'D':
			werrlog(WERRCODE(52004),*option,0,0,0,0,0,0,0);
			break;
		default:
			werrlog(WERRCODE(52004),*option,0,0,0,0,0,0,0);
			break;
	}
}											

static int4 do_vtoc_find( char *lib, char *vol, int4 *start, int4 *count, char *rcvr, int4 *total)
{
	char *big_rcvr;								/* Pointer to malloc reciever area for FIND().	*/
	char *br, *dr;								/* Pointers to big_rcvr and destination rcvr.	*/
	int	i;
	int4	l_count;							/* Local copy of "count" variable.		*/

	l_count = WL_get_swap(count);
	big_rcvr = wisp_malloc( (int)(22 * l_count) );				/* Allocate memory for FIND() results.		*/

	WL_set_va_count(8);
	FIND( "?       ", lib, vol, start, count, big_rcvr, total, "A" );	/* Get "count" number of file names from FIND().*/

	l_count = WL_get_swap(count);						/* Get actual number of file names returned.	*/
	br = big_rcvr + 14;							/* File name begins at character position 14.	*/
	dr = rcvr;								/* Set destination reciever to first position.	*/
	for( i = 0 ; i < l_count ; ++i )					/* For each file name returned from FIND().	*/
	{
		memcpy( dr, br, 8 );						/* Copy 8 character file name to rcvr.		*/
		br += 22;							/* FIND() rcvr is 22 bytes int4 (vol,lib,name).	*/
		dr += 8;							/* Next destination is after 8 byte file name.	*/
	}

	free( big_rcvr );							/* Free allocated reciever memory space.	*/

	return l_count;
}


/*
**	History:
**	$Log: readvtoc.c,v $
**	Revision 1.22  2003/02/17 22:07:17  gsl
**	move VSSUB prototypes to vssubs.h
**	
**	Revision 1.21  2003/01/31 18:48:36  gsl
**	Fix  copyright header and -Wall warnings
**	
**	Revision 1.20  2002/12/10 20:54:12  gsl
**	use WERRCODE()
**	
**	Revision 1.19  2002/12/09 21:09:30  gsl
**	Use WL_wtrace(ENTRY)
**	
**	Revision 1.18  2002/07/12 17:00:59  gsl
**	Make WL_ global unique changes
**	
**	Revision 1.17  2002/07/02 21:15:28  gsl
**	Rename wstrdup
**	
**	Revision 1.16  2002/06/25 15:21:53  gsl
**	Change to use wmalloc()
**	
**	Revision 1.15  2002/06/21 03:10:39  gsl
**	Remove VMS & MSDOS
**	
**	Revision 1.14  1998/07/31 19:43:37  gsl
**	Changed NAME_LENGTH to COB_FILEPATH_LEN
**	
**	Revision 1.13  1997-05-01 16:38:49-04  gsl
**	Remove unneeded buff
**
**	Revision 1.12  1997-04-15 23:10:41-04  gsl
**	Update to use wtrace()
**
**	Revision 1.11  1997-03-12 13:02:22-05  gsl
**	changed to use WIN32 define
**
**	Revision 1.10  1996-08-19 18:32:47-04  gsl
**	drcs update
**
**
**
*/
