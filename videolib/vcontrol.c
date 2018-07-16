static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
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
int v_control_data=FALSE;

int vcontrol(char *string)								/* Output a control string.		*/
{											
	int i;

	if (NULL==string)
	{
		/*
		**	A NULL means to flush the output
		*/
		return vcontrol_flush();
	}
	if (!*string)
	{
		/*
		**	Empty string - do nothing
		*/
		return SUCCESS;
	}

	v_control_data = TRUE;								/* set the flag                         */
	if (vdeferred())								/* Should we do any output?		*/
	{
		vre("vcontrol()-Output attempted when in deferred state, string = %s",string+1);
		v_control_data=FALSE;							/* will be output later, so clear for now*/
		return(FAILURE);							/* Return failure.			*/
	}

	vrawprint(string);								/* Print the data.			*/
	if (vcap_padding) for(i=0; i<vcap_padding; ++i) vrawputc((char)0);
	v_control_data = FALSE;								/* clear the control flag               */
	return(SUCCESS);
}

/*
**	Flush the control data out
*/
int vcontrol_flush(void)
{
	int ret;

	v_control_data = TRUE;
	ret=vrawflush();
	v_control_data=FALSE;
	return ret;
}


int vcap_reset_terminal(void)
{
#ifdef	DIRECTVID
	if (vrawdirectio())
	{
		return SUCCESS;
	}
#endif
	return vcontrol(vcapvalue(RESET_TERMINAL));
}
int vcap_init_terminal(void)
{
#ifdef	DIRECTVID
	if (vrawdirectio())
	{
		return SUCCESS;
	}
#endif
	return vcontrol(vcapvalue(INIT_TERMINAL));
}

/*
**	History:
**	$Log: vcontrol.c,v $
**	Revision 1.13  1997-07-12 17:32:31-04  gsl
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
