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
#include "vlocal.h"
#include "vdata.h"

/*						Subroutine entry point.								*/

void VREADFIELDS(comarea) struct vplus_comarea *comarea;
{
	if (vformcurrent->start_row == 0)						/* Zero biased?				*/
	{
		if (vformcurrent->window_line == 0) vformcurrent->start_row = 1;	/* Move past message window.		*/
	}
	vformcurrent->control = FORM_READ_ONLY;						/* Make it read only.			*/
	vp_term_key = vform(vformcurrent,vform_data_buffer,window_message);

	comarea->numerrs = 0;
	comarea->lastkey = vp_term_key;
}
