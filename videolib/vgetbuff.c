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

void VGETBUFFER(comarea,data,length) struct vplus_comarea *comarea; char data[]; int2 *length;
{
	int i,j,len;
	char *wp, *dp;
	int dat;

	if ((*length < 0) || (*length > 1920))
	{
		vre_window("VGETBUFFER: %d is not a valid buffer length.",*length);
		vexit(0);
		exit(0);
	}

	for (i = 0, wp = vform_data_buffer, dp = data; i < *length; i++) *dp++ = *wp++;		/* Copy the data.		*/

	for (i = 0, dp = data; i < vformcurrent->fields; i++)					/* Check every field.		*/
	{
		len = vformcurrent->field[i].length;						/* Make working copy.		*/
		dat = vformcurrent->field[i].datatype;
		if ((dat == FIELD_DATA_DIG) || (dat == FIELD_DATA_NUM) || (dat == FIELD_DATA_IMP))
		{
			if ((*dp == '+') || (*dp == '-'));					/* Sign already left justified?	*/
			else									/* No, then check for sign.	*/
			{
				for (j = 1; j < len; j++)					/* Loop through the string.	*/
				{
					if ((*(dp+j) == '+') || (*(dp+j) == '-'))		/* A leading sign?		*/
					{
						*dp = *(dp+j);					/* Yes so left justify.		*/
						*(dp+j) = '0';					/* Put in a leading zero.	*/
					}
				}
			}

			wp = dp;								/* Point to the data.		*/
			for (j = 0; j < len; j++)						/* Loop through each char.	*/
			{
				if (*wp == ' ') *wp = '0';					/* Convert spaces to 0.		*/
				wp++;								/* Next character.		*/
			}

			if (*dp == '-')								/* Special case of minus zero.	*/
			{
				int mz;								/* Minus zero flag.		*/
				mz = TRUE;							/* Assume a minus zero.		*/
				for (j = 1; (j < len) && mz; j++)				/* Loop through all chars.	*/
				{
					if (*(dp+j) != '0') mz = FALSE;				/* Was it a zero?		*/
				}
				if (mz) *dp = '0';						/* Yes so change to leading 0.	*/
			}
		}
		dp = dp + vformcurrent->field[i].length;					/* Move to next field's data.	*/
	}
}
