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

			/************************************************************************/
			/*									*/
			/*	     VIDEO - Video Interactive Development Environment		*/
			/*									*/
			/*			    Copyright (c) 1987				*/
			/*									*/
			/*	An unpublished work by Greg L. Adams.  All rights reserved.	*/
			/*									*/
			/************************************************************************/


/*					Include required header files.								*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "video.h"									/* Get the standard video definitions.	*/
#include "vcap.h"
#include "vintdef.h"
#include "vmodules.h"
#include "vdata.h"
#include "vraw.h"

/*					Subroutine entry point.									*/

/* the following global flag is set when this routine is used for output        */
/* the flag is used by vrawunix to decide whether or not to attempt translation */
/* (IVS/Chinese) to output data.  Data coming from this routine should NOT be   */
/* so translated                                                                */
int VL_v_control_data=FALSE;

int VL_vcontrol(char *string)								/* Output a control string.		*/
{											
	int i;

	if (NULL==string)
	{
		/*
		**	A NULL means to flush the output
		*/
		return VL_vcontrol_flush();
	}
	if (!*string)
	{
		/*
		**	Empty string - do nothing
		*/
		return SUCCESS;
	}

	VL_v_control_data = TRUE;								/* set the flag                         */
	if (vdeferred())								/* Should we do any output?		*/
	{
		vre("vcontrol()-Output attempted when in deferred state, string = %s",string+1);
		VL_v_control_data=FALSE;							/* will be output later, so clear for now*/
		return(FAILURE);							/* Return failure.			*/
	}

	vrawprint(string);								/* Print the data.			*/
	if (VL_vcap_padding) for(i=0; i<VL_vcap_padding; ++i) vrawputc((char)0);
	VL_v_control_data = FALSE;								/* clear the control flag               */
	return(SUCCESS);
}

/*
**	Flush the control data out
*/
int VL_vcontrol_flush(void)
{
	int ret;

	VL_v_control_data = TRUE;
	ret=vrawflush();
	VL_v_control_data=FALSE;
	return ret;
}


int VL_vcap_reset_terminal(void)
{
#ifdef	DIRECTVID
	if (vrawdirectio())
	{
		return SUCCESS;
	}
#endif
	return VL_vcontrol(VL_vcapvalue(RESET_TERMINAL));
}
int VL_vcap_init_terminal(void)
{
#ifdef	DIRECTVID
	if (vrawdirectio())
	{
		return SUCCESS;
	}
#endif
	return VL_vcontrol(VL_vcapvalue(INIT_TERMINAL));
}

/*
**	History:
**	$Log: vcontrol.c,v $
**	Revision 1.19  2003/06/20 15:48:03  gsl
**	VL_ globals
**	
**	Revision 1.18  2003/06/20 15:37:44  gsl
**	VL_ globals
**	
**	Revision 1.17  2003/01/31 19:25:56  gsl
**	Fix copyright header
**	
**	Revision 1.16  2002/07/15 20:56:38  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.15  2002/07/15 20:16:07  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.14  2002/07/15 17:52:54  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.13  1997/07/12 21:32:31  gsl
**	Add missing includes
**	
**	Revision 1.12  1997-07-08 16:52:07-04  gsl
**	Add vcap_reset_terminal() and vcap_init_terminal()
**	Fix for vrawdirectio() and COSTAR
**
**	Revision 1.11  1996-11-13 20:47:05-05  gsl
**	Added vcontrol_flush() to replace vcontrol(DUMP_OUTPUT/NULL)
**	vcontrol() now does nothing for an empty string.
**	vcontrol_flush() calls vrawflush()
**
**	Revision 1.10  1996-10-11 15:16:02-07  gsl
**	drcs update
**
**
**
*/
