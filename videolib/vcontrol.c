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

#include "video.h"									/* Get the standard video definitions.	*/
#include "vcap.h"

/*					Subroutine entry point.									*/

/* the following global flag is set when this routine is used for output        */
/* the flag is used by vrawunix to decide whether or not to attempt translation */
/* (IVS/Chinese) to output data.  Data coming from this routine should NOT be   */
/* so translated                                                                */
int v_control_data=FALSE;

int vcontrol(string) unsigned char *string;						/* Output a control string.		*/
{											
	extern int deferred;								/* Output control flags.		*/
	int i,ret;

	v_control_data = TRUE;								/* set the flag                         */
	if (string == DUMP_OUTPUT) 
	{
		ret=vrawprint(DUMP_OUTPUT);						/* Should the buffer be dumped?		*/
		v_control_data=FALSE;							/* clear the flag now                   */
		return ret;
	}
	else if (deferred)								/* Should we do any output?		*/
	{
		vre("vcontrol()-Output attempted when in deferred state, string = %s",string+1);
		v_control_data=FALSE;							/* will be output later, so clear for now*/
		return(FAILURE);							/* Return failure.			*/
	}

	vrawprint(string);								/* Print the data.			*/
	if (padding) for(i=0; i<padding; ++i) vrawputc((char)0);
	v_control_data = FALSE;								/* clear the control flag               */
}
