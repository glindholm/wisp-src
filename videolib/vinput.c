/*
******************************************************************************
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
**
** NOTICE:
** Confidential, unpublished property of Shell Stream Software LLC.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of Shell Stream Software LLC
** is strictly prohibited.
** 
******************************************************************************
*/


/*						Include standard header files.							*/

#include <stdio.h>									/* Include standard I/O.		*/
#include <time.h>
#include "video.h"									/* Include definitions.			*/
#include "vlocal.h"									/* Include local definitions.		*/
#define VIDEO_DATA_ROOT 1								/* This is the video data root.		*/
#include "vcap.h"
#include "vdata.h"									/* Include internal database.		*/
#include "vmodules.h"

/*						Data declarations.								*/

static unsigned char VL_refresh_character = REFRESH_CHARACTER;				/* Refresh the screen on this character.	*/
static unsigned char trigger_character = TRIGGER_CHARACTER;				/* Bring up the triggered event.		*/

static char char_pushed = 0;								/* Push-back character.			*/
static char vgc(int wait);
static int vgc_control(char c);

/*						Get a single character (wait for it).						*/

char VL_vgetc(void)
{
	return(vgc(1));									/* Call common get char routine.	*/
}

char VL_vcheck(void)
{
	return(vgc(0));									/* Call the commmon routine.		*/
}

static char vgc(int wait)          							/* Wait / nowait flag.			*/
{
	char c;										/* Working character storage.		*/

	if ((c = char_pushed)) 								/* Is there a pushed character?		*/
	{
		char_pushed = CHAR_NULL;						/* Yes, so we have it now.		*/
		return(c);								/* Return the character.		*/
	}

	if ((c = VL_paste_buffer[VL_paste_index])) 					/* Anything in the paste buffer?	*/
	{
		VL_paste_index++;							/* Yes, then move to the next char.	*/
		if (VL_paste_buffer[VL_paste_index] == CHAR_NULL)			/* Was this the last one?		*/
		{
			VL_paste_index = 0;						/* Yes, then reset the index.		*/
			VL_paste_buffer[VL_paste_index] = CHAR_NULL;			/* Clear out the buffer.		*/
		}
		return(c);								/* Return to the caller.		*/
	}

	if (VL_macro_input != NULL)							/* Is a macro input file active?	*/
	{
		if ((c = getc(VL_macro_input)) != EOF)					/* Get macro input.			*/
		{
			getc(VL_macro_input);						/* Throw away the trailing CR.		*/
			return(c);							/* Return the character.		*/
		}
		else
		{
			fclose(VL_macro_input);						/* Close the input file.		*/
			VL_macro_input = NULL;						/* Clear the file pointer.		*/
		}
	}

	vdefer_restore();								/* Restore if action deferred.		*/
	if (VL_vb_count) VL_vcontrol_flush();						/* And dump all buffers.		*/

	if (wait)									/* Are we in wait mode?			*/
	{
		while (vgc_control(c = vrawinput()));					/* Get input.				*/
	}

	else
	{
try_again:	c = vrawcheck();							/* None, so get input.			*/
		if (c != CHAR_NULL)
		{
			if (vgc_control(c)) goto try_again;				/* Repeat if input controlable.		*/
		}
	}

	if ((VL_macro_output != NULL) && (c != CHAR_NULL))					/* Write out a macro?			*/
	{
		fprintf(VL_macro_output, "%c\n", c);					/* Output the macro character.		*/
	}

	return(c);									/* Return what we got.			*/
}
/*						Perform control W processing.							*/

static int vgc_control(char c)
{
	if ((c == VL_refresh_character) && (voptlevel() != VOP_OFF))			/* Is this the refresh queue char?	*/
	{
		if (VL_macro_input == NULL)						/* Don't do if reading a macro.		*/
		{
			VL_vrefresh(HARD_REFRESH);						/* Hard refresh the full screen.	*/
			VL_vcontrol_flush();						/* Make sure the buffer dumps.		*/
		}
		return(TRUE);								/* Return reporting that we did it.	*/
	}
	else if ((c == trigger_character) && (voptlevel() != VOP_OFF))			/* Is this the trigger character?	*/
	{
		return(VL_vtrigger());							/* Yes, then should it be processed?	*/
	}
	else return(FALSE);								/* Didn't refresh the screen.		*/
}

/*						Push a character back.								*/

int VL_vpushc(char char_to_be_pushed)
{
	if (char_pushed) return(FAILURE);						/* Char already pushed?			*/
	char_pushed = char_to_be_pushed;						/* No, then push it.			*/
	return(SUCCESS);								/* And all ok.				*/
}

/*						Get a character allowing timeout.						*/

char VL_vgetcto(int timer)
{
	long sttime, endtime;								/* System time value.			*/
	int c;

	sttime = time(NULL);
	endtime = sttime + timer;							/* Get the current time.		*/

	for( c=0 ; (c==0) && (time(NULL)<endtime) ;)
	{
		c = vcheck();
		if (c==0)
		{
			VL_vwait(1,0);							/* Sleep 1 second			*/
		}
	}
	return(c);									/* Return what we got, 0 for timeout.	*/
}

/*
	Video Input Error routines

		Video was not designed to handle raw input errors; this is a post-design addition to handle errors
		that occurs on a raw read of the terminal.

	VL_vseterr()
		If an error occurs while doing a raw read then vseterr() is called with a non-zero error number.
		It is expected that vseterr() will only be called with a non-zero value because a zero will clear the error
		flag before vgeterr() has a chance to retrieve it.  Use vgeterr() to clear a previous error.

	VL_vgeterr()
		This routine is called to retrieve the last error number or zero if there was no error.  It will also
		clear the error flag.
*/

static int g_verror = 0;							/* The last error number flag			*/

void VL_vseterr(int error)								/* Set the error number flag			*/
   	      									/* The error number (non-zero)			*/
{
	g_verror = error;
}

int VL_vgeterr(void)								/* Get the error number flag			*/
{
	int	error;

	error = g_verror;
	g_verror = 0;								/* Clear the error number flag			*/
	return(error);								/* Return the error number			*/
}

/*
**	History:
**	$Log: vinput.c,v $
**	Revision 1.19  2003/01/31 20:35:57  gsl
**	Fix -Wall warnings
**	
**	Revision 1.18  2003/01/31 19:25:56  gsl
**	Fix copyright header
**	
**	Revision 1.17  2002/07/17 21:06:02  gsl
**	VL_ globals
**	
**	Revision 1.16  2002/07/16 13:40:22  gsl
**	VL_ globals
**	
**	Revision 1.15  2002/07/15 20:16:09  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.14  2002/07/15 17:10:03  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.13  1997/07/08 21:01:32  gsl
**	Change to use vwait()
**	Change to use new video.h interfaces
**	
**	Revision 1.12  1996-11-13 20:30:24-05  gsl
**	Use VL_vcontrol_flush()
**
**	Revision 1.11  1996-10-11 15:16:06-07  gsl
**	drcs update
**
**
**
*/
