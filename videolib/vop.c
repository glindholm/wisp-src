			/************************************************************************/
			/*									*/
			/*	     VIDEO - Video Interactive Development Environment		*/
			/*									*/
			/*			    Copyright (c) 1987				*/
			/*									*/
			/*	An unpublished work by Greg L. Adams.  All rights reserved.	*/
			/*									*/
			/************************************************************************/


/*						Include standard header files.							*/

#include "video.h"									/* Include video definitions.		*/


/*						Subroutine entry point.								*/

voptimize(how) int how;
{
	extern int optimization;							/* Optimization control flag.		*/
	register int ret;								/* Working registers.			*/

	switch(how)
	{
		case OFF:
		case TRACKING_ONLY:
		case DATA_ONLY:
		case DATA_AND_CONTROLS:
		case DATA_CONTROLS_AND_MOTION:
		case DEFER_MODE:
		case BLOCK_MODE:
		{
			ret = optimization;						/* Remember the old optimization.	*/
			vdefer(RESTORE);						/* No, so restore from deferred action.	*/
			optimization = how;						/* Select the optimization.		*/
			break;								/* We're all done.			*/
		}

		default:								/* Oops, caller doesn't know...		*/
		{
			vre("voptimize(%d)-Invalid parameter",how);			/* Report the error.			*/
			return(FAILURE);						/* And return in shame.			*/
			break;
		}
	}
	return(ret);
}
