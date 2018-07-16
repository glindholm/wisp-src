			/************************************************************************/
			/*	      VIDEO - Video Interactive Development Environment		*/
			/*			Copyright (c) 1988, 1989, 1990			*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/

/*						Include definitions.								*/

#include "video.h"
#include "vlocal.h"
#include "vdata.h"

/*						Subroutine entry point.								*/

vbell()
{
	register int temp;								/* Working register.			*/

	vdefer(RESTORE);								/* Bell is cause end deferred action.	*/
	temp = optimization;								/* Remember the optimization level.	*/
	optimization = OFF;								/* Turn off optimization so bell rings.	*/
#ifdef MSDOS
	vcontrol(DUMP_OUTPUT);								/* Dump the buffer.			*/
	printf("\007");									/* Output a bell character.		*/
#else	/* VMS and unix */
	vcontrol("\007");								/* Output a bell character.		*/
	vcontrol(DUMP_OUTPUT);								/* Dump the buffer.			*/
#endif	/* VMS and unix */
	optimization = temp;								/* Restore the optimization.		*/
	return(SUCCESS);								/* What could go wrong.			*/
}
