			/************************************************************************/
			/*	     VIDEO - Video Interactive Development Environment		*/
			/*			Copyright (c) 1987-1993				*/
			/*	An unpublished work by International Digital Scientific Inc.	*/
			/*			  All rights reserved.				*/
			/************************************************************************/

#include <stdio.h>									/* Include header files.		*/
#include "video.h"									/* Include the video database.		*/
#include "vform.h"									/* Include the view database.		*/
#include "vintdef.h"
#include "vplus.h"

/*						Subroutine entry point.								*/

void VCLOSETERM(comarea) struct vplus_comarea *comarea;
{
	verase(FULL_SCREEN);
	vmove(23,0);
	vdefer(RESTORE);
	return;
}
