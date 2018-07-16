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

static float value();
extern char *vgetinit();

void VPUTBUFFER(comarea,data,length) struct vplus_comarea *comarea; char data[]; int2 *length;
{
	int i,j,len,dat,ini;
	char *wp, *dp;

	if ((*length > 1920) || (*length < 0))
	{
		vre_window("VPUTBUFFER: %d is not a valid size for the length parameter.",*length);
		vexit(0);
		exit(0);
	}

	for (i = 0, wp = vform_data_buffer, dp = data; i < *length; i++) *wp++ = *dp++;		/* Copy the data.		*/

	for (i = 0, dp = vform_data_buffer; i < vformcurrent->fields; i++)			/* Check every field.		*/
	{
		len = vformcurrent->field[i].length;						/* Make working copy.		*/
		dat = vformcurrent->field[i].datatype;
		ini = vformcurrent->field[i].init_data_chars;

		if ((dat == FIELD_DATA_DIG) || (dat == FIELD_DATA_NUM) || (dat == FIELD_DATA_IMP))
		{
			if (value(dp,len) == 0.0)						/* Is the field all zeroes?	*/
			{
				wp = dp;							/* Working pointer.		*/
				for (j = 0; j < len; j++) *wp++ = ' ';				/* Set to spaces.		*/
			}
		}

		if (vfblank(dp,len) && ini)							/* Is the field blank?		*/
		{
			wp = vgetinit(current_form,i);						/* Get the init data.		*/
			for (j = 0; (j < len) && (j < ini); j++) *(dp+j) = *(wp+j);		/* Copy the data.		*/
			if ((dat == FIELD_DATA_DIG) || (dat == FIELD_DATA_NUM) || (dat == FIELD_DATA_IMP)) jredit(dp,len);
		}

		dp = dp + vformcurrent->field[i].length;					/* Move to next field's data.	*/
	}
}

static float value(dx,len) char *dx; int len;
{
	int i;
	float x;
	char buff[128];
	float numval();

	for (i = 0; i < len; i++) buff[i] = *dx++;						/* Copy the buffer.		*/
	buff[i] = CHAR_NULL;									/* Null terminate.		*/
	x = numval(buff);
	return(x);									/* Get the integer value.	*/
}
