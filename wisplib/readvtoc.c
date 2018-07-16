			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

#include "werrlog.h"

#define		ROUTINE		52000

#ifdef VMS
#include <descrip.h>
#include <rmsdef.h>
#include "wfiles.h"
#include "wcommon.h"

char *wfname();

readvtoc(option, lib, vol, start, end, rcvr, rtrn_code, count)

char *option, *lib, *vol, *rcvr;
long  *start, *end, *count, *rtrn_code;

{											
 	char *l_rcvr, *ptr;                                        
	long l_start, l_end, find_status, l_rtrn_code;
	int  i, j, k, c;
	char *context;
	long mode;

	char template[NAME_LENGTH], result[256];

$DESCRIPTOR(t_desc, template);
$DESCRIPTOR(r_desc, result);

	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);						/* Say we are here.			*/

	i = 0;
	j = 0;
                      
	l_start = *start;								/* Get local copies.			*/
	l_end   = *end;
	l_rcvr  = rcvr;

	wswap(&l_start);								/* Swap the word order.			*/
	wswap(&l_end);

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
	} while ((find_status == RMS$_NORMAL) && (i < l_end));				/* While there's more files and they	*/
											/* Want more files.			*/

	wswap(&c); 									/* Swap the word order.			*/
 	*count = c;									/* Set the arg.				*/

	if ((find_status == RMS$_NORMAL) || (find_status == RMS$_NMF))			/* Did we complete normally ?		*/
	{
		l_rtrn_code = 0;							/* Yup.  Set the return code.		*/
	}
	else
	{
		l_rtrn_code = 24;							/* Nope.  Set the return code.		*/
	}

	wswap(&l_rtrn_code);								/* Swap the word order.			*/
	*rtrn_code = l_rtrn_code;							/* And set the argument.		*/

	find_status = LIB$FIND_FILE_END(&context);					/* And free the context area.		*/

}
#endif	/* #ifdef VMS */

#ifndef VMS	/* unix or MSDOS */

#include <malloc.h>
#include <string.h>
#include <memory.h>

READVTOC(option, lib, vol, start, end, rcvr, rtrn_code, count)
char *option, *lib, *vol, *rcvr;
long  *start, *end, *count, *rtrn_code;
{
	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);						/* Say we are here.			*/

	switch( toupper(*option) )
	{
		case 'F':								/* The "F" option is like a FIND call.	*/
		{
			do_vtoc_find( lib, vol, start, end, rcvr, count );

			rtrn_code = 0L;
			wswap( &rtrn_code );

			return( 0 );
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

static	do_vtoc_find( lib, vol, start, end, rcvr, count)
char *lib, *vol, *rcvr;
long  *start, *end, *count;
{
	char *big_rcvr;								/* Pointer to malloc reciever area for FIND().	*/
	char *br, *dr;								/* Pointers to big_rcvr and destination rcvr.	*/
	int	i;
	long	l_end;								/* Local copy of "end" variable.		*/

	l_end = *end;								/* Get maximum number of file names to recieve.	*/
	wswap( &l_end );							/* Swap bytes so l_end is correct for C.	*/
	big_rcvr = malloc( 22 * l_end );					/* Allocate memory for FIND() results.		*/

	FIND( "?       ", lib, vol, start, end, big_rcvr, count, "A" );		/* Get "end" number of file names from FIND().	*/

	l_end = *end;								/* Get actual number of file names returned.	*/
	wswap( &l_end );							/* Swap bytes so l_end is correct for C.	*/
	br = big_rcvr + 14;							/* File name begins at character position 14.	*/
	dr = rcvr;								/* Set destination reciever to first position.	*/
	for( i = 0 ; i < l_end ; ++i )						/* For each file name returned from FIND().	*/
	{
		memcpy( dr, br, 8 );						/* Copy 8 character file name to rcvr.		*/
		br += 22;							/* FIND() rcvr is 22 bytes long (vol,lib,name).	*/
		dr += 8;							/* Next destination is after 8 byte file name.	*/
	}

	free( big_rcvr );							/* Free allocated reciever memory space.	*/
}

#endif	/* #ifdef unix or MSDOS */

