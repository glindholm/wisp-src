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

vrelease()										/* Release from optimization.		*/
{
	extern int new_screen;								/* Reference release control flag.	*/

	vdefer(RESTORE);								/* Restore from current optimization.	*/
	new_screen = TRUE;								/* Flag that next screen is new.	*/
	return(SUCCESS);								/* Return to the caller.		*/
}
