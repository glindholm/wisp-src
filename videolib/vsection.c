/*
******************************************************************************
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
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
******************************************************************************
*/

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
**	VL_vbuffering()
*/

/*
**	Includes
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "video.h"
#include "vmodules.h"
#include "vdata.h"

/*
**	Structures and Defines
*/

/*
**	Globals and Externals
*/

/*
**	Static data
*/
static int nesting_level = 0;			/* Output buffering control flag.	*/


/*
**	Static Function Prototypes
*/
static int VL_vbuffering(int state);

int VL_vbuffering_start(void)
{
	return VL_vbuffering(VBUFF_START);
}
int VL_vbuffering_end(void)
{
	return VL_vbuffering(VBUFF_END);
}


/*
**	ROUTINE:	VL_vbuffering()
**
**	FUNCTION:	Turn buffering on or off.
**
**	DESCRIPTION:	Call VL_vbuffering(VBUFF_START) to begin a series of video ouput commands
**			then call VL_vbuffering(VBUFF_END) to identify the end of the series.
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
static int VL_vbuffering(int state)		/* Select type of buffering operation.	*/
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
		nesting_level += 1;					/* Increment the holding control flag.	*/
	}
	else if (VBUFF_END == state) /* AUTOMATIC */			/* Are we in automatic operation.	*/
	{
		nesting_level -= 1;					/* Decrement the holding control flag.	*/

		if ((nesting_level == 0) && (VL_vb_count != 0)) 	/* Did we get to zero?			*/
		{
			VL_vcontrol_flush();
		}
		else if (nesting_level < 0)				/* Did we go below zero?		*/
		{
			vre("Internal error in vbuffering(), nesting_level flag decremented below 0.");
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

/*
 * int VL_vbuffering_on():	
 *			This flag is non-zero when the higher layers of
 *			Video wish to postpone flushing output waiting in the
 *			buffer until a "logical sequence" of operations is
 *			complete.
*/

int VL_vbuffering_on(void)
{
	return nesting_level;
}

/*
**	History:
**	$Log: vsection.c,v $
**	Revision 1.17  2003/01/31 19:25:55  gsl
**	Fix copyright header
**	
**	Revision 1.16  2002/07/17 21:06:04  gsl
**	VL_ globals
**	
**	Revision 1.15  2002/07/16 13:40:20  gsl
**	VL_ globals
**	
**	Revision 1.14  2002/07/15 17:52:56  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.13  1997/07/09 16:17:50  gsl
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
