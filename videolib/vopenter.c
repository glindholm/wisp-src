			/************************************************************************/
			/*	     VIDEO - Video Interactive Development Environment		*/
			/*			Copyright (c) 1987-1993				*/
			/*	An unpublished work by International Digital Scientific Inc.	*/
			/*			  All rights reserved.				*/
			/************************************************************************/

#include <stdio.h>									/* Include header files.		*/
#include "video.h"									/* Include the video database.		*/
#include "vintdef.h"									/* Include the COBOL interface defs.	*/
#include "vplus.h"									/* Include the view database.		*/

void VOPENTERM(comarea,termfile) struct vplus_comarea *comarea; unsigned char *termfile;
{
	comarea->cstatus = 0;									/* Just fill and return.	*/
	comarea->filerrnum = 0;
	comarea->filen = 1;
	comarea->identifier = 0;
	comarea->labinfo = 8;
	return;
}
