			/************************************************************************/
			/*	     VIDEO - Video Interactive Development Environment		*/
			/*			Copyright (c) 1987-1993				*/
			/*	An unpublished work by International Digital Scientific Inc.	*/
			/*			  All rights reserved.				*/
			/************************************************************************/

#include <stdio.h>									/* Include header files.		*/
#include "video.h"									/* Include the video database.		*/
#include "vintdef.h"									/* Include integer definitions.		*/

/*						Subroutine entry point.								*/

void VLOADFORMS(comarea, num, loaded, forms) struct vplus_comarea *comarea; int2 *num, *loaded; char *forms;
{
	*loaded = 0;								/* Say we loaded none.			*/
	return;										/* All done.				*/
}
