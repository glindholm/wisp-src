			/************************************************************************/
			/*	     VIDEO - Video Interactive Development Environment		*/
			/*			Copyright (c) 1987-1993				*/
			/*	An unpublished work by International Digital Scientific Inc.	*/
			/*			  All rights reserved.				*/
			/************************************************************************/

#include <stdio.h>									/* Include header files.		*/
#include "video.h"									/* Include the video database.		*/
#include "vform.h"									/* Include the view database.		*/
#include "vintdef.h"									/* Define remote integers.		*/
#include "vplus.h"
#include "vlocal.h"
#include "vdata.h"

/*						Subroutine entry point.								*/

void VGETFIELDINFO(comarea,infobuf,infobuflen)
	struct vplus_comarea *comarea; struct vplus_infobuf *infobuf; int2 *infobuflen;
{
	int i,j;
	char formname[16], fieldname[16];
	int ne, el;
	struct video_form *cf;
	int onum,fnum;

	comarea->cstatus = 0;								/* Clear the comarea status.		*/

	for (i = 0; i < 16; i++)
	{
		formname[i] = infobuf->form_name[i];
	}

	if (!veqfn(formname,vformcurrent->name))					/* Is this the current form?		*/
	{
		vre_window("VGETFIELDINFO: Only info for the current form is supported.");
		comarea->cstatus = -1;
		return;
	}
	else cf = vformcurrent;

	ne = infobuf->number_of_entries;						/* Get the number of entries.		*/
	el = infobuf->entry_length;

	for (i = 0; i < ne; i++)							/* Loop through each entry.		*/
	{
		for (j = 0; j < 16; j++) fieldname[j] = infobuf->entry_table[i].field_name[j];
		if ((onum = onbyname(fieldname)) == -1)					/* Did we get a valid order number?	*/
		{
			onum = infobuf->entry_table[i].order_number - 1;		/* Then did they give us one?		*/
			if ((onum >= 0) && (onum < cf->fields));			/* Is it valid?				*/
			else
			{
				vre_window("VGETFIELDINFO: Cannot retreive field info by field number");
				comarea->cstatus = -1;
				return;
			}
		}

		if (el >= 8)								/* Anything to return?			*/
		{
			for (j = 0; j < 16; j++) infobuf->entry_table[i].field_name[j] = fieldname[j];
			if (el >=  9) infobuf->entry_table[i].order_number = onum + 1;	/* Return the order number.		*/
			if (el >= 10) infobuf->entry_table[i].field_number = cf->field[onum].number;
			if (el >= 11) vre_window("VGETFIELDINFO: Entry length must be 10 or less."); 
		}
	}
}

