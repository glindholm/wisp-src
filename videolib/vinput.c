static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*	      VIDEO - Video Interactive Development Environment		*/
			/*			Copyright (c) 1988, 1989, 1990			*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/

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

static char char_pushed = 0;								/* Push-back character.			*/
static char vgc(int wait);
static int vgc_control(char c);

/*						Get a single character (wait for it).						*/

char vgetc(void)
{
	char vgc();									/* Working storage.			*/
	return(vgc(1));									/* Call common get char routine.	*/
}

char vcheck(void)
{
	char vgc();									/* Define vgc routine.			*/
	return(vgc(0));									/* Call the commmon routine.		*/
}

static char vgc(int wait)          							/* Wait / nowait flag.			*/
{
	char c;										/* Working character storage.		*/

	if (c = char_pushed) 								/* Is there a pushed character?		*/
	{
		char_pushed = CHAR_NULL;						/* Yes, so we have it now.		*/
		return(c);								/* Return the character.		*/
	}

	if (c = paste_buffer[paste_index]) 						/* Anything in the paste buffer?	*/
	{
		paste_index++;								/* Yes, then move to the next char.	*/
		if (paste_buffer[paste_index] == CHAR_NULL)				/* Was this the last one?		*/
		{
			paste_index = 0;						/* Yes, then reset the index.		*/
			paste_buffer[paste_index] = CHAR_NULL;				/* Clear out the buffer.		*/
		}
		return(c);								/* Return to the caller.		*/
	}

	if (macro_input != NULL)							/* Is a macro input file active?	*/
	{
		if ((c = getc(macro_input)) != EOF)					/* Get macro input.			*/
		{
			getc(macro_input);						/* Throw away the trailing CR.		*/
			return(c);							/* Return the character.		*/
		}
		else
		{
			fclose(macro_input);						/* Close the input file.		*/
			macro_input = NULL;						/* Clear the file pointer.		*/
		}
	}

	vdefer_restore();								/* Restore if action deferred.		*/
	if (vb_count) vcontrol_flush();							/* And dump all buffers.		*/

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

	if ((macro_output != NULL) && (c != CHAR_NULL))					/* Write out a macro?			*/
	{
		fprintf(macro_output, "%c\n", c);					/* Output the macro character.		*/
	}

	return(c);									/* Return what we got.			*/
}
/*						Perform control W processing.							*/

static int vgc_control(char c)
{
	if ((c == refresh_character) && (voptlevel() != VOP_OFF))			/* Is this the refresh queue char?	*/
	{
		if (macro_input == NULL)						/* Don't do if reading a macro.		*/
		{
			vrefresh(HARD_REFRESH);						/* Hard refresh the full screen.	*/
			vcontrol_flush();						/* Make sure the buffer dumps.		*/
		}
		return(TRUE);								/* Return reporting that we did it.	*/
	}
	else if ((c == trigger_character) && (voptlevel() != VOP_OFF))			/* Is this the trigger character?	*/
	{
		return(vtrigger());							/* Yes, then should it be processed?	*/
	}
	else return(FALSE);								/* Didn't refresh the screen.		*/
}

/*						Push a character back.								*/

int vpushc(char char_to_be_pushed)
{
	if (char_pushed) return(FAILURE);						/* Char already pushed?			*/
	char_pushed = char_to_be_pushed;						/* No, then push it.			*/
	return(SUCCESS);								/* And all ok.				*/
}

/*						Get a character allowing timeout.						*/

char vgetcto(int timer)
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
			vwait(1,0);							/* Sleep 1 second			*/
		}
	}
	return(c);									/* Return what we got, 0 for timeout.	*/
}

/*
	Video Input Error routines

		Video was not designed to handle raw input errors; this is a post-design addition to handle errors
		that occurs on a raw read of the terminal.

	vseterr()
		If an error occurs while doing a raw read then vseterr() is called with a non-zero error number.
		It is expected that vseterr() will only be called with a non-zero value because a zero will clear the error
		flag before vgeterr() has a chance to retrieve it.  Use vgeterr() to clear a previous error.

	vgeterr()
		This routine is called to retrieve the last error number or zero if there was no error.  It will also
		clear the error flag.
*/

static int g_verror = 0;							/* The last error number flag			*/

void vseterr(int error)								/* Set the error number flag			*/
   	      									/* The error number (non-zero)			*/
{
	g_verror = error;
}

int vgeterr(void)								/* Get the error number flag			*/
{
	int	error;

	error = g_verror;
	g_verror = 0;								/* Clear the error number flag			*/
	return(error);								/* Return the error number			*/
}

/*
**	History:
**	$Log: vinput.c,v $
**	Revision 1.13  1997-07-08 17:01:32-04  gsl
**	Change to use vwait()
**	Change to use new video.h interfaces
**
**	Revision 1.12  1996-11-13 20:30:24-05  gsl
**	Use vcontrol_flush()
**
**	Revision 1.11  1996-10-11 15:16:06-07  gsl
**	drcs update
**
**
**
*/
