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

static char * pp;
static int step();

/*						Subroutine entry point.								*/

char *vgetinit(form,field) int form,field;
{
	int i,j,k,done;
	struct video_form *dp;

	pp = vinitdata;
	dp = vformdata;
	done = FALSE;

	for (i = 0; (i <= form) && !done; i++)
	{
		for (j = 0; (j < dp->fields) && !done; j++)
		{
			if ((i == form) && (j == field)) done = TRUE;
			else if (dp->field[j].init_data_chars) step();
		}
		dp++;
	}
	return(pp);
}

char *vgetproc(form,field,level) int form, field, level;
{
	int i,j,k,done;
	struct video_form *dp;								/* Working pointer.			*/

	pp = vformproc;									/* Point to the data buffer.		*/
	dp = vformdata;
	done = FALSE;

	for (i = 0; (i <= form) && !done; i++)						/* Check every form.			*/
	{
		for (j = 0; (j < dp->fields) && !done; j++)				/* Check every field.			*/
		{
			if ((i == form) && (j == field)) done = TRUE;			/* At the field?			*/
			else
			{
				for (k = 0; k < dp->field[j].init_processing_lines; k++) step();
				for (k = 0; k < dp->field[j].field_processing_lines; k++) step();
				for (k = 0; k < dp->field[j].finish_processing_lines; k++) step();
			}
		}
		dp++;
	}

	if (dp != vformdata) dp--;							/* Go back if we overran.		*/
	if (level >= 1) for (k = 0; k < dp->field[field].init_processing_lines; k++) step();
	if (level == 2) for (k = 0; k < dp->field[field].field_processing_lines; k++) step();

	return(pp);
}

static int step()
{
	while (*pp != CHAR_NULL) pp++;							/* Go to the end of the last one.	*/
	pp++;										/* Go to the start of the next one.	*/
}
