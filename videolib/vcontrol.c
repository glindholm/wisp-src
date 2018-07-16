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

int vcontrol(string) unsigned char *string;						/* Output a control string.		*/
{
	extern int deferred;								/* Output control flags.		*/
	int i;
	
	if (string == DUMP_OUTPUT) return(vrawprint(DUMP_OUTPUT));			/* Should the buffer be dumped?		*/

	else if (deferred)								/* Should we do any output?		*/
	{
		vre("vcontrol()-Output attempted when in deferred state, string = %s",string+1);
		return(FAILURE);							/* Return failure.			*/
	}

	vrawprint(string);								/* Print the data.			*/
	if (padding) for(i=0; i<padding; ++i) vrawputc((char)0);
}
