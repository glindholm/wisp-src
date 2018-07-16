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
**	File:		wsxio.c
**
**	Project:	wisp/lib
**
**	RCS:		$Source:$
**
**	Purpose:	Emulate the WSXIO VSSUB.
**
**	Routines:	
**	WSXIO()
*/

/*
**	Includes
*/

#include <string.h>
#include <stdarg.h>

#include "idsistd.h"
#include "vwang.h"									/* Reference workstation emulation.	*/
#include "werrlog.h"
#include "wisplib.h"

/*
**	Structures and Defines
*/

/*
88002	%%WSXIO-E-NOTSUPP Unsupported XIO operation [%d]
88004	%%WSXIO-E-NOTSUPP Unsupported function [%c]
88006	%%WSXIO-E-BADLEN Invalid mapping area length [%d]
88008	%%WSXIO-E-NOTSUPP Feature not supported [%s]
*/

#define OPEN 	'O'									/* Define WSXIO options - Open.		*/
#define CLOSE 	'C'									/*			- Close.	*/
#define AID 	'A'									/*			- I/D char.	*/
#define XIO 	'X'									/*			- Do I/O.	*/
#define TXIO 	'T'									/*			- Do Timed I/O.	*/
#define WAIT 	'W'									/*			- Char. waot.	*/
#define XIO_READ 		0x40							/* XIO I/O operations	- Read.		*/
#define XIO_READ_ALTERED 	0x50							/*			- Read altered.	*/
#define XIO_WRITE 		0x80							/*			- Write.	*/
#define XIO_WRITE_SELECTED 	0x90							/*			- Write select.	*/


/*
	WSXIO(arg1,  arg2,  arg3,  arg4,  arg5,  arg6,  arg7,  arg8,  arg9,  arg10,  arg11)
	WSXIO("O",   Device,X,     Ufb,   X)
	WSXIO("C",   Ufb)
	WSXIO("X",   Ufb,   Cmd,   Oa,   [Oalen,] Map, Maplen, Iosw,  X,     X,      X) 
	WSXIO("T",   Ufb,   Cmd,   Oa,   [Oalen,] Map, Maplen, Iosw,  Timer)
	WSXIO("W",   Ufb,   Htime, Iosw)
	WSXIO("A",   Ufb,   Aid)
*/
int WSXIO(const char *function, ...)
{
	va_list the_args;
	void *next_arg;

	unsigned char *command;
	unsigned char *order_area;
	unsigned char *mapping_area;
	int4 *ma_length;
	unsigned char *iosw;
	int4 *timer;

	unsigned char vw_function[1], *vw_wsb, vw_lines[1];				/* vwang parameters.			*/
	int4	map_len;
	uint4 timeout, l;
	char pfkey_pressed[40];	
	unsigned char vwang_scr[WSB_LENGTH+20];
	unsigned char vw_no_mod[2];
	unsigned char aid_char;
	
	WL_wtrace("WSXIO","ENTRY","Entry into WSXIO(%c)", *function);

	va_start(the_args, function);

	vw_no_mod[0] = ' ';

	switch (function[0])								/* Select the workstation function.	*/
	{
		case OPEN:								/* Open the workstation.		*/
		{
			break;								/* Nothing more to do.			*/
		}
		case CLOSE:								/* Close the workstation.		*/
		{
			vw_function[0] = CLOSE_WORK_STATION;
			vw_lines[0]  = (unsigned char)0;
			vwang(vw_function,(unsigned char*)"\001\000\000\000",vw_lines,"X",NULL,NULL);
			break;								/* And we're all done.			*/
		}
		case WAIT:								/* Wait for a character.		*/
		{
			int4	sec100, seconds;

			next_arg = va_arg(the_args, void*);	/* Arg2 - skip */
			next_arg = va_arg(the_args, void*);	/* Arg3 - Htime */

			sec100 = WL_get_swap((int4*)next_arg);				/* Get timeout period.			*/
			if ( sec100 < 0 )						/* Less then zero means no timeout	*/
				seconds = 0;
			else if ( sec100 == 0 )						/* No Wait -- instant timeout		*/
				seconds = 0;
			else if ( sec100 < 100 ) 					/* Minimum wait is 1 second		*/
				seconds = 1;
			else
				seconds = (sec100+50) / 100;				/* In seconds, not hundreths.		*/
			iosw = va_arg(the_args, unsigned char*);/* Arg4 - IOSW */

			if (vwang_aid() != AID_UNLOCKED)
			{
				/*
				**	If keyboard is not unlocked then nothing to wait for, just return.
				*/
				return(0);
			}

			if ( seconds < 1 )
			{
				/*
				**	We don't allow this because we are not modeling a client/server.
				**	Input only occurs on a read so an instant timeout would never allow
				**	input and would likely setup an infinite loop.
				*/
				werrlog(WERRCODE(88008),"WAIT without TIMEOUT",0,0,0,0,0,0,0);
				return(0);
			}

			/*
			**	To compensate for not modelling a client/server, a WAIT will perform a timed READ instead.
			**	On the Wang the WAIT would be waiting for a user to finish inputing data. This is only
			**	allowed on a READ in our emulation.
			*/

			vw_function[0] = READ_MODIFIABLE;
			memset(vwang_scr,' ',WSB_LENGTH);
			vwang_scr[OA_ROW] = (unsigned char)1;
			vw_lines[0]  = (unsigned char)WSB_ROWS;
			vwang_timeout(seconds);						/* Set the timer.			*/
			vwang(vw_function,vwang_scr,vw_lines,"A",pfkey_pressed,vw_no_mod);	/* Go do the I/O action.		*/
			vwang_timeout((int4)0);						/* Cancel timer				*/

			aid_char = vwang_aid();						/* Get the current AID char		*/
			if (aid_char != AID_UNLOCKED)					/* If keyboard is locked then		*/
			{
				/*
				**	Only if the read terminates do we set the
				**	aid char in the iosw, otherwise the iosw
				**	is unchanged.
				*/
				iosw[2] = aid_char;					/* Set the AID char in IOSW		*/
			}
			break;								/* All done.				*/
		}
		case XIO:								/* Perform I/O operations.		*/
		case TXIO:
		{
			next_arg = va_arg(the_args, void*);				/* Arg2 - skip */
			command = va_arg(the_args, unsigned char*);			/* Arg3 - CMD */
			order_area = va_arg(the_args, unsigned char*);			/* Arg4 - OA */

			next_arg = va_arg(the_args, void*);				/* Arg5 - OALEN - optional */
			l = WL_get_swap((int4*)next_arg);
			if ( l == 4 )
			{
				mapping_area 	= va_arg(the_args, unsigned char*);	/* Arg6 - mapping area */;
			}
			else
			{
				mapping_area 	= (unsigned char*)next_arg;		/* Arg6 - mapping area */
			}
			ma_length    	= va_arg(the_args, int4*);			/* Arg7 - MAP LEN */
			iosw         	= va_arg(the_args, unsigned char*);		/* Arg8 - IOSW */
			timer		= va_arg(the_args, int4*);			/* Arg9 - TIMER */

			switch(command[0])						/* Determine the I/O operation.		*/
			{
			case XIO_READ:  
				vw_function[0] = READ_MODIFIABLE;  			/* Read & change pseudoblanks	*/
				break;
			case XIO_WRITE: 
				vw_function[0] = WRITE_ALL; 				/*   internatl functionality.		*/
				break;
			case XIO_READ_ALTERED: 
				vw_function[0] = READ_ALTERED; 
				break;
			case XIO_WRITE_SELECTED: 
				vw_function[0] = WRITE_SELECTED; 
				break;
			default: 
				werrlog(WERRCODE(88002),(int)(command[0]),0,0,0,0,0,0,0);
				return(0);
			}

			map_len = WL_get_swap(ma_length);				/* Get the length of the mapping area.	*/
			if (map_len > (WSB_LENGTH-OA_LENGTH) || map_len < 0)		/* Validate map length			*/
			{
				werrlog(WERRCODE(88006),map_len,0,0,0,0,0,0,0);
				return(0);
			}
			vw_lines[0] = (map_len > 0) ? (map_len / WSB_COLS) : 0;		/* Determine the number of lines.	*/
			vw_wsb = &vwang_scr[0];						/* Point to the mapping area.		*/
			memcpy(vw_wsb,order_area,OA_LENGTH);				/* Now copy the order area in.		*/
			memcpy(&vwang_scr[OA_LENGTH],mapping_area,(int)map_len);
			
			if (function[0] == TXIO)					/* If A TIMED READ			*/
			{
				timeout = WL_get_swap(timer);
				vwang_timeout(timeout);					/* Set the timer.			*/
			}

			vwang(vw_function,vw_wsb,vw_lines,"A",pfkey_pressed,vw_no_mod);		/* Go do the I/O action.		*/

			if (command[0] == XIO_WRITE || command[0] == XIO_WRITE_SELECTED)
			{
				vwang_flush();						/* Bring screen up to date.		*/
			}

			memcpy(order_area,vw_wsb,OA_LENGTH);				/* Return the modified one.		*/
			memcpy(mapping_area,&vwang_scr[OA_LENGTH],(int)map_len);

			iosw[2] = (unsigned char)vwang_aid();				/* Assign the AID char to the IOSW	*/
			
			if (function[0] == TXIO) vwang_timeout((int4)0);		/* Clear timer if needed.		*/
			break;								/* Whew, we're done.			*/
		}

		case AID:								/* Attention ID character.		*/
		{
			/*
			**	Get the last AID character, including reading if unlocked.
			**	This functionality used to be inline here but has been moved to vwang.
			**
			**	To compensate for not modelling a client/server we check to see if
			**	there are keystrokes waiting to be read and if so we do a 1 second Wait.
			*/
			unsigned char* aid;
			next_arg = va_arg(the_args, void*);	/* Arg2 - skip */
			aid = va_arg(the_args, unsigned char*);	/* Arg3 - AID */

			*aid = vwang_aid_read_unlocked();
			break;								/* Exit.				*/
		}

		default:
		{
			werrlog(WERRCODE(88004),function[0],0,0,0,0,0,0,0);
			return(0);
		}
	}
	va_end(the_args);
	return(0);
}

/*
**	History:
**	$Log: wsxio.c,v $
**	Revision 1.20  2003/01/31 19:08:36  gsl
**	Fix copyright header  and -Wall warnings
**	
**	Revision 1.19  2003/01/29 18:59:01  gsl
**	Change to use stdargs.h
**	
**	Revision 1.18  2002/12/10 20:54:05  gsl
**	use WERRCODE()
**	
**	Revision 1.17  2002/12/09 21:09:35  gsl
**	Use WL_wtrace(ENTRY)
**	
**	Revision 1.16  2002/08/01 14:45:09  gsl
**	type warnings
**	
**	Revision 1.15  2002/07/12 17:01:08  gsl
**	Make WL_ global unique changes
**	
**	Revision 1.14  1998/07/10 15:31:10  gsl
**	Initialize the no_mod field to vwang()
**	
**	Revision 1.13  1996-07-15 13:25:11-04  gsl
**	Fix for NT
**
**	Revision 1.12  1995-04-25 02:55:09-07  gsl
**	drcs state V3_3_15
**
 * Revision 1.11  1995/04/17  11:48:05  gsl
 * drcs state V3_3_14
 *
 * Revision 1.10  1995/03/07  11:33:44  gsl
 * replace literals with defines
 *
 * Revision 1.9  1995/02/14  17:00:52  gsl
 * remove the video includes.
 * Move the AID function logic to vwang_aid_read_unlocked() in vwang.c
 * and replace it with a call the this routine.
 *
 * Revision 1.8  1995/02/14  16:07:30  gsl
 * Change the CLOSE function to a call vwang(CLOSE_WORK_STATION),
 * and change a vdefer(RESTORE) to a call to vwang_flash()
 * (thats vwang_flush())
 *
**
**
*/
