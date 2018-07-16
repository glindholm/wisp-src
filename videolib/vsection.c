static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
/*
**	File:		vsection.c
**
**	Project:	video
**
**	RCS:		$Source:$
**
**	Purpose:	Control video output buffering
**
**	Routines:	
**	vbuffering()
*/

/*
**	Includes
*/
#include <stdio.h>									/* Include the standard I/O system.	*/
#include <stdlib.h>
#include <string.h>

#include "video.h"									/* Reference WOW definitions.		*/
#include "vmodules.h"

/*
**	Structures and Defines
*/

/*
**	Globals and Externals
*/
extern int vb_count;									/* Data in buffer counter.		*/

/*
**	Static data
*/
static int holding_output = 0;								/* Output buffering control flag.	*/


/*
**	Static Function Prototypes
*/

/*
 * int vbuffering_on():	This flag is non-zero when the higher layers of
 *			Video wish to postpone flushing output waiting in the
 *			buffer until a "logical sequence" of operations is
 *			complete.
*/

int vbuffering_start(void)
{
	return vbuffering(VBUFF_START);
}
int vbuffering_end(void)
{
	return vbuffering(VBUFF_END);
}


/*
**	ROUTINE:	vbuffering()
**
**	FUNCTION:	Turn buffering on or off.
**
**	DESCRIPTION:	Call vbuffering(VBUFF_START) to begin a series of video ouput commands
**			then call vbuffering(VBUFF_END) to identify the end of the series.
**			The output commands between the start and end will output all together.
**			You can nest the start and end calls but every start must have a
**			matching end call.
**
**			For debugging you can disable vbuffering by setting environment
**			variable VBUFFERING=OFF.
**
**	ARGUMENTS:	
**	state		VBUFF_START or VBUFF_END
**
**	GLOBALS:	?
**
**	RETURN:		?
**
**	WARNINGS:	Every start must have a matching end.
**
*/
int vbuffering(int state)								/* Select type of buffering operation.	*/
{
	static int first = 1;
	static int buffering = 1;
	
	if (first)
	{
		char	*ptr;
		
		first = 0;
		ptr = getenv("VBUFFERING");
		if (ptr && 0 == strcmp(ptr,"OFF"))
		{
			buffering = 0;
		}
	}

	if (!buffering)
	{
		return SUCCESS;
	}
	
	if (VBUFF_START == state) /* LOGICAL */
	{
		holding_output += 1;							/* Increment the holding control flag.	*/
	}
	else if (VBUFF_END == state) /* AUTOMATIC */					/* Are we in automatic operation.	*/
	{
		holding_output -= 1;							/* Decrement the holding control flag.	*/

		if ((holding_output == 0) && (vb_count != 0)) 				/* Did we get to zero?			*/
		{
			vcontrol_flush();	
		}
		else if (holding_output < 0)						/* Did we go below zero?		*/
		{
			vre("Internal error in vbuffering(), holding_output flag decremented below 0.");
			exit(FAILURE);
		}
	}
	else
	{
		vre("Internal error in vbuffering(), state parameter not one of VBUFF_START/LOGICAL VBUFF_END/AUTOMATIC ");
		exit(FAILURE);
	}
	return(SUCCESS);
}

int vbuffering_on(void)
{
	return holding_output;
}

/*
**	History:
**	$Log: vsection.c,v $
**	Revision 1.13  1997-07-09 12:17:50-04  gsl
**	Add new interface.
**
**	Revision 1.12  1996-11-13 20:32:17-05  gsl
**	use vcontrol_flush()
**
**	Revision 1.11  1996-07-18 11:50:35-07  jockc
**	added include string.h for strcmp()
**
**	Revision 1.10  1996-03-12 05:15:12-08  gsl
**	Changed LOGICAL/AUTOMATIC to VBUFF_START/VBUFF_END.
**	Documented.
**	Added debug flag VBUFFERING=OFF for turning off buffering via an env var.
**
**
**
*/
