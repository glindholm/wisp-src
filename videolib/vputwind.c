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

void VPUTWINDOW(comarea,message,length) struct vplus_comarea *comarea; unsigned char message[]; int2 *length;
{
	int i;

	for (i = 0; i < *length; i++)
	{
		window_message[i] = message[i];
	}
	window_message[i] = CHAR_NULL;
}
