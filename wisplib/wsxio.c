static char copyright[]="Copyright (c) 1988-1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
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

#include "idsistd.h"
#include "vwang.h"									/* Reference workstation emulation.	*/
#include "werrlog.h"
#include "wisplib.h"

/*
**	Structures and Defines
*/

#define		ROUTINE		88000
/*
88001	%%WSXIO-I-ENTRY Entry into WSXIO [%c]
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
	WSXIO("O", Device, X, Ufb, X)
	WSXIO("C", Ufb)
	WSXIO("X", Ufb, Cmd, Oa, [Oalen,] Map, Maplen, Iosw, X, X, X) 
	WSXIO("T", Ufb, Cmd, Oa, [Oalen,] Map, Maplen, Iosw, Timer)
	WSXIO("W", Ufb, Htime, Iosw)
	WSXIO("A", Ufb, Aid)
*/
int WSXIO(     arg1,  arg2,  arg3,  arg4,  arg5,  arg6,  arg7,  arg8,  arg9,  arg10,  arg11)
unsigned char *arg1, *arg2, *arg3, *arg4, *arg5, *arg6, *arg7, *arg8, *arg9, *arg10, *arg11;
{
	unsigned char *function;
	unsigned char *command;
	unsigned char *order_area;
	unsigned char *mapping_area;
	uint4 *ma_length;
	unsigned char *iosw;
	unsigned char *timer;

	unsigned char vw_function[1], *vw_wsb, vw_lines[1];				/* vwang parameters.			*/
	int4	map_len;
	uint4 timeout, l;
	unsigned char dummy[40];							/* Dummy storage.			*/
	unsigned char vwang_scr[WSB_LENGTH+20];
	char vw_no_mod[2];
	unsigned char	aid_char;
	
	werrlog(ERRORCODE(1),*arg1,0,0,0,0,0,0,0);

	function   = arg1;

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
			vwang(vw_function,"\001\000\000\000",vw_lines,"X",NULL,NULL);
			break;								/* And we're all done.			*/
		}
		case WAIT:								/* Wait for a character.		*/
		{
			int4	sec100, seconds;

			iosw = arg4;
			memcpy(&sec100,arg3,sizeof(int4));				/* Get timeout period.			*/
			wswap(&sec100);
			if ( sec100 < 0 )						/* Less then zero means no timeout	*/
				seconds = 0;
			else if ( sec100 == 0 )						/* No Wait -- instant timeout		*/
				seconds = 0;
			else if ( sec100 < 100 ) 					/* Minimum wait is 1 second		*/
				seconds = 1;
			else
				seconds = (sec100+50) / 100;				/* In seconds, not hundreths.		*/

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
				werrlog(ERRORCODE(8),"WAIT without TIMEOUT",0,0,0,0,0,0,0);
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
			vwang(vw_function,vwang_scr,vw_lines,"A",dummy,vw_no_mod);	/* Go do the I/O action.		*/
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
			command = arg3;
			order_area = arg4;

			memcpy(&l,arg5,sizeof(int4));
			wswap(&l);
			if ( l == 4 )
			{
				mapping_area 	= arg6;
				ma_length    	= (uint4 *) arg7;
				iosw         	= arg8;
				timer		= arg9;
			}
			else
			{
				mapping_area 	= arg5;
				ma_length    	= (uint4 *) arg6;
				iosw         	= arg7;
				timer		= arg8;
			}

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
				werrlog(ERRORCODE(2),(int)(command[0]),0,0,0,0,0,0,0);
				return(0);
			}

			memcpy(&map_len,ma_length,sizeof(int4));			/* Get the length of the mapping area.	*/
			wswap(&map_len);
			if (map_len > (WSB_LENGTH-OA_LENGTH) || map_len < 0)		/* Validate map length			*/
			{
				werrlog(ERRORCODE(6),map_len,0,0,0,0,0,0,0);
				return(0);
			}
			vw_lines[0] = (map_len > 0) ? (map_len / WSB_COLS) : 0;		/* Determine the number of lines.	*/
			vw_wsb = &vwang_scr[0];						/* Point to the mapping area.		*/
			memcpy(vw_wsb,order_area,OA_LENGTH);				/* Now copy the order area in.		*/
			memcpy(&vwang_scr[OA_LENGTH],mapping_area,(int)map_len);
			
			if (function[0] == TXIO)					/* If A TIMED READ			*/
			{
				memcpy((char *)&timeout,(char *)timer,sizeof(int4));
				wswap(&timeout);
				vwang_timeout(timeout);					/* Set the timer.			*/
			}

			vwang(vw_function,vw_wsb,vw_lines,"A",dummy,vw_no_mod);		/* Go do the I/O action.		*/

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

			*arg3 = vwang_aid_read_unlocked();
			break;								/* Exit.				*/
		}

		default:
		{
			werrlog(ERRORCODE(4),function[0],0,0,0,0,0,0,0);
			return(0);
		}
	}
	return(0);
}

/*
**	History:
**	$Log: wsxio.c,v $
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
