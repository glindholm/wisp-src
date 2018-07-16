			/************************************************************************/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/

/*						Include required header files.							*/

#include <v/video.h>									/* Reference the VIDEO library.		*/
#include <v/vlocal.h>									/* Needed for vdata.h header.		*/
#include <v/vdata.h>									/* For enter_key and return_key defs.	*/
#include "vwang.h"									/* Reference workstation emulation.	*/

/*						Local definitions.								*/

#define OPEN 'O'									/* Define WSXIO options - Open.		*/
#define CLOSE 'C'									/*			- Close.	*/
#define AID 'A'										/*			- I/D char.	*/
#define XIO 'X'										/*			- Do I/O.	*/
#define TXIO 'T'									/*			- Do Timed I/O.	*/
#define WAIT 'W'									/*			- Char. waot.	*/
#define XIO_READ 0x40									/* XIO I/O operations	- Read.		*/
#define XIO_READ_ALTERED 0x50								/*			- Read altered.	*/
#define XIO_WRITE 0x80									/*			- Write.	*/
#define XIO_WRITE_SELECTED 0x90								/*			- Write select.	*/

extern char wsx_buf;									/* Global buffer ptr for wsx io		*/
static char vw_no_mod[2];

/*						Subroutine entry point.								*/

WSXIO(         arg1,  arg2,  arg3,  arg4,  arg5,  arg6,  arg7,  arg8,  arg9,  arg10,  arg11)
unsigned char *arg1, *arg2, *arg3, *arg4, *arg5, *arg6, *arg7, *arg8, *arg9, *arg10, *arg11;
{
	unsigned char *function;
	unsigned char *command;
	unsigned char *order_area;
	unsigned long *oa_length;
	unsigned char *mapping_area;
	unsigned long *ma_length;
	unsigned char *iosw;


	char c, vgetc();								/* A working character.			*/
	unsigned char vw_function[1], *vw_wsb, vw_lines[1];				/* vwang parameters.			*/
	char oa_save[4],oa_temp[4];							/* Order area save location.		*/
	register char *k;								/* Working pointer.			*/
	long i,x;									/* Working counter.			*/
	unsigned long timeout, l;
	unsigned char dummy[40];							/* Dummy storage.			*/
	unsigned char vwang_scr[2000];
	unsigned char *tmp;
	
	function   = arg1;

	switch (function[0])								/* Select the workstation function.	*/
	{
		case OPEN:								/* Open the workstation.		*/
		{
			break;								/* Nothing more to do.			*/
		}
		case CLOSE:								/* Close the workstation.		*/
		{
			vdefer(RESTORE);						/* Short cut, bring screen up to date.	*/
			vshut();							/* Shut down all I/O.			*/
			break;								/* And we're all done.			*/
		}
		case WAIT:								/* Wait for a character.		*/
		{
			long	sec100, seconds;

			memcpy(&sec100,arg3,4);						/* Get timeout period.			*/
			wswap(&sec100);
			if ( sec100 < 0 )						/* Less then zero means no timeout	*/
				seconds = 0;
			else if ( sec100 == 0 )						/* No Wait -- instant timeout		*/
				seconds = 0;
			else if ( sec100 < 100 ) 					/* Minimum wait is 1 second		*/
				seconds = 1;
			else
				seconds = (sec100+50) / 100;				/* In seconds, not hundreths.		*/

			vkbtimer(seconds);						/* Set the timer.			*/

			if ( sec100 < 0 )
				c = vgetc();						/* No timeout				*/
			else
				c = vcheck();						/* Wait with timeout			*/

			if (c)								/* Got one.				*/
			{
				vpushc(c);						/* Now put the character back.		*/
				i = vgetm();
				x = (long)vfnkey((int)i);				/* What pfkey did they press ?		*/
				if (x) wsx_buf = x + ((x > 16) ? 80 : 64);		/* Return the correct AID byte value.	*/
				else							/* Not a PF-key from 1 to 32.		*/
				{
					if ( (enter_key == i) || (return_key == i) )	/* Is it the ENTER or RETURN key?	*/
					{
						wsx_buf = '@';				/* If so, return '@' sign.		*/
					}
				}
			}
			else
			{
				wsx_buf = 0;						/* No AID char.				*/
			}

			arg4[2] = wsx_buf;						/* Put into the IOSW.			*/

			vkbtimer(0L);							/* Clear timer.				*/

			break;								/* All done.				*/
		}
		case XIO:								/* Perform I/O operations.		*/
		case TXIO:
		{
			command = arg3;
			order_area = arg4;

			memcpy(&l,arg5,4);
			wswap(&l);
			if ( l == 4 )
			{
				mapping_area = arg6;
				ma_length    = (unsigned long *) arg7;
				iosw         = arg8;
			}
			else
			{
				mapping_area = arg5;
				ma_length    = (unsigned long *) arg6;
				iosw         = arg7;
			}

			if (function[0] == TXIO)
			{
				memcpy(&timeout,iosw,4);
				wswap(&timeout);
				vkbtimer(timeout);					/* Set the timer.			*/
			}

			switch(command[0])						/* Determine the I/O operation.		*/
			{
				case XIO_READ:  { vw_function[0] = READ_MODIFIABLE;  break; }	/* Read & change pseudoblanks	*/
				case XIO_WRITE: { vw_function[0] = WRITE_ALL; break; }	/*   internatl functionality.		*/
				case XIO_READ_ALTERED: { vw_function[0] = READ_ALTERED; break; }
				case XIO_WRITE_SELECTED: { vw_function[0] = WRITE_SELECTED; break; }
				default: { vre("WSXIO - Invalid XIO operation %d", command[0]); break; }
			}
			memcpy(&i,ma_length,4);						/* Get the length of the mapping area.	*/
			wswap(&i);							/* Convert to VAX terms.		*/
			vw_lines[0] = i / 80;						/* Determine the number of lines.	*/
			vw_wsb = &vwang_scr[0];						/* Point to the mapping area.		*/
			memcpy(vw_wsb,order_area,4);					/* Now copy the order area in.		*/
			memcpy(&vwang_scr[4],mapping_area,(int)(i>1920?1920:i));
			
			vwang(vw_function,vw_wsb,vw_lines,"A",dummy,vw_no_mod);		/* Go do the I/O action.		*/

			if (command[0] == XIO_WRITE || command[0] == XIO_WRITE_SELECTED)
			{
				vdefer(RESTORE);					/* Bring screen up to date.		*/
			}

			wsx_buf = vw_no_mod[1];						/* Save AID byte.			*/

			memcpy(order_area,vw_wsb,4);					/* Return the modified one.		*/
			memcpy(mapping_area,&vwang_scr[4],(int)(i>1920?1920:i));
			tmp = iosw;
			tmp[2] = vw_no_mod[1];
			
			if (function[0] == TXIO) vkbtimer(0L);				/* Clear timer if needed.		*/
			break;								/* Whew, we're done.			*/
		}

		case AID:								/* Attention ID character.		*/
		{
			if (!wsx_buf)							/* No AID char.				*/
			{
				c = vcheck();						/* Wait for a character to come in.	*/
				if (c)
				{
					vpushc(c);					/* Now put the character back.		*/
					i = vgetm();
					x = (long)vfnkey((int)i);			/* What pfkey did they press ?		*/
					if (x) wsx_buf = x + ((x > 16) ? 80 : 64);	/* Return the correct AID byte value.	*/
					else						/* Not a PF-key from 1 to 32.		*/
					{
						if ( (enter_key == i) 
							|| (return_key == i) )		/* Is it the ENTER or RETURN key?	*/
						{
							wsx_buf = '@';			/* If so, return '@' sign.		*/
						}
					}
				}
			}

			*arg3 = wsx_buf;						/* Return the last AID character or NULL*/
			wsx_buf = 0;
			break;								/* Exit.				*/
		}

		default:
		{
			vre("WSXIO - Unsupported function %c",function[0]);		/* Report the error.			*/
			break;								/* But carry on.			*/
		}
	}
	return(1);									/* Return a success code.		*/
}
