static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988,1989,1990,1991,1992,1993,1994		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/


#include "idsistd.h"
#include "werrlog.h"
#include "movebin.h"
#include "wfname.h"
#include "wisplib.h"

#define		ROUTINE		52000

#ifdef VMS
#include <descrip.h>
#include <rmsdef.h>
#include "wfiles.h"
#include "wcommon.h"


void READVTOC(char *option, char *lib, char *vol, int4 *start, int4 *count, char *rcvr, int4 *rtrn_code, int4 *total)
{											
 	char *l_rcvr, *ptr;                                        
	int4 l_start, l_count, find_status, l_rtrn_code;
	int  i, j, k, c;
	char *context;
	int4 mode;
	char template[NAME_LENGTH], result[256];
#include "readvtoc.d"

	werrlog(ERRORCODE(1),*option,lib,vol,0,0,0,0,0);				/* Say we are here.			*/

	i = 0;
	j = 0;
                      
	l_start = *start;								/* Get local copies.			*/
	l_count = *count;
	l_rcvr  = rcvr;

	wswap(&l_start);								/* Swap the word order.			*/
	wswap(&l_count);

	mode = 0;
	mode |= IS_LIB;
	
	ptr = wfname( &mode, vol, lib, "        ", template );				/* Let wfname make a name for itself.	*/

	*ptr++ = '*';									/* Add on wild cards.			*/
	*ptr++ = '.';
	*ptr++ = '*';
	*ptr++ = ';';
	*ptr++ = '*';

	i = 0;
	c = 0;                                                         
	context = 0;
                                                     
	do
	{             
		find_status = LIB$FIND_FILE(&t_desc, &r_desc, &context, 0, 0, 0, 0);	/* Is there anybody out there.		*/

		if (find_status == RMS$_NORMAL)						/* Success ?				*/
		{
 			c++;								/* Bump up the count.			*/
	      		memset(l_rcvr, ' ', 8);						/* Clear out 8 characters.		*/
			j = strpos(result, "]");					/* Find this character.			*/
			j++;								/* Bump up to the next character.	*/
			if (j != -1)							/* Is there a filename ?		*/
			{
				k = 0;							/* Initial character count.		*/
				do
				{
					*l_rcvr++ = result[j++];			/* Move a character.			*/
					k++;						/* And up the count.			*/
				} while ((k < 8) && (result[j] != '.'));		/* Until the '.' or 8 characters.	*/
			}
			k = 8 - k;							/* We may not have moved 8 characters.	*/
			l_rcvr += k;							/* Maybe we gotta update the pointer.	*/
		}                                                                                                                 

		i++;									/* Up the iteration count.		*/ 
	} while ((find_status == RMS$_NORMAL) && (i < l_count));			/* While there's more files and they	*/
											/* Want more files.			*/

	if ((find_status == RMS$_NORMAL) || (find_status == RMS$_NMF))			/* Did we complete normally ?		*/
	{
		l_rtrn_code = (c > 0) ? 0 : 24;						/* Yup.  Set the return code.		*/
	}
	else
	{
		l_rtrn_code = 24;							/* Nope.  Set the return code.		*/
	}

	wswap(&c); 									/* Swap the word order.			*/
 	*total = c;									/* Set the arg.				*/

	wswap(&l_rtrn_code);								/* Swap the word order.			*/
	*rtrn_code = l_rtrn_code;							/* And set the argument.		*/

	find_status = LIB$FIND_FILE_END(&context);					/* And free the context area.		*/

}
#endif	/* VMS */

#if defined(unix) || defined(MSDOS) || defined(WIN32)

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static int4 do_vtoc_find( char *lib, char *vol, int4 *start, int4 *count, char *rcvr, int4 *total);

void READVTOC(char *option, char *lib, char *vol, int4 *start, int4 *count, char *rcvr, int4 *rtrn_code, int4 *total)
{

	werrlog(ERRORCODE(1),*option,lib,vol,0,0,0,0,0);				/* Say we are here.			*/

	switch( toupper(*option) )
	{
		case 'F':								/* The "F" option is like a FIND call.	*/
		{
			int4	x_count, x_total, x_rc;

			do_vtoc_find( lib, vol, start, count, rcvr, total );

			GETBIN(&x_count, count, 4);
			GETBIN(&x_total, total, 4);
			wswap(&x_count);
			wswap(&x_total);

			x_rc =  (x_count > 0) ? 0 : 24;
			PUTBIN(rtrn_code, &x_rc, 4);
			wswap( rtrn_code );

			wtrace("READVTOC","RETURN","Count=%ld Total=%ld RC=%ld.",
						(long)x_count, (long)x_total, (long)x_rc);
			break;
		}
		case 'L':
		case 'V':
			werrlog(ERRORCODE(2),*option,0,0,0,0,0,0,0);
			break;
		case 'G':
		case 'X':
		case '#':
		case 'D':
			werrlog(ERRORCODE(4),*option,0,0,0,0,0,0,0);
			break;
		default:
			werrlog(ERRORCODE(4),*option,0,0,0,0,0,0,0);
			break;
	}
}											

static int4 do_vtoc_find( char *lib, char *vol, int4 *start, int4 *count, char *rcvr, int4 *total)
{
	char *big_rcvr;								/* Pointer to malloc reciever area for FIND().	*/
	char *br, *dr;								/* Pointers to big_rcvr and destination rcvr.	*/
	int	i;
	int4	l_count;							/* Local copy of "count" variable.		*/
	int4	argcnt;

	GETBIN(&l_count, count, 4);						/* Get maximum number of file names to recieve.	*/
	wswap( &l_count );							/* Swap bytes so l_count is correct for C.	*/
	big_rcvr = malloc( (int)(22 * l_count) );				/* Allocate memory for FIND() results.		*/

	argcnt = 8;
	wvaset(&argcnt);
	FIND( "?       ", lib, vol, start, count, big_rcvr, total, "A" );	/* Get "count" number of file names from FIND().*/

	GETBIN(&l_count, count, 4);						/* Get actual number of file names returned.	*/
	wswap( &l_count );							/* Swap bytes so l_count is correct for C.	*/
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

#endif	/* unix || MSDOS || WIN32 */

/*
**	History:
**	$Log: readvtoc.c,v $
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
