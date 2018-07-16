					/****************************************************/
					/*						    */
					/*		 WINDOWS ON THE WORLD		    */
					/*						    */
					/*	Copyright (c) 1989 by Gregory L. Adams      */
					/*     	         All rights reserved!		    */
					/*						    */
					/****************************************************/


/*					Definitions.										*/

#include <stdio.h>									/* Include the standard I/O system.	*/
#include "video.h"									/* Reference WOW definitions.		*/

/*					Subroutine entry point.									*/

int vbuffering(state) int state;							/* Select type of buffering operation.	*/
{
	extern int holding_output;							/* Output buffering control flag.	*/
	extern int vb_count;								/* Data in buffer counter.		*/

	if (state == LOGICAL) holding_output = holding_output + 1;			/* Increment the holding control flag.	*/
	else if (state == AUTOMATIC)							/* Are we in automatic operation.	*/
	{
		holding_output = holding_output - 1;					/* Decrement the holding control flag.	*/
		if ((holding_output == 0) && (vb_count != 0)) vcontrol(DUMP_OUTPUT);	/* Did we get to zero?			*/
		else if (holding_output < 0)						/* Did we go below zero?		*/
		{
			vre("Internal error in vbuffering(), holding_output flag decremented below 0.");
			exit(FAILURE);
		}
	}
	else
	{
		vre("Internal error in vbuffering(), state parameter not one of AUTOMATIC or LOGICAL");
		exit(FAILURE);
	}
	return(SUCCESS);
}
