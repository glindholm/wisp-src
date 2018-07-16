			/************************************************************************/
			/*	     VIDEO - Video Interactive Development Environment		*/
			/*			Copyright (c) 1987-1993				*/
			/*	An unpublished work by International Digital Scientific Inc.	*/
			/*			  All rights reserved.				*/
			/************************************************************************/


#ifdef VMS										/* Include VMS specific headers.	*/
#include <descrip.h>									/* Include descriptor layout for VMS.	*/
#include <ssdef.h>									/* Include VMS system messages.		*/
#endif

#ifndef unix										/* Include DOS & VMS headers.		*/
#include <stdlib.h>									/* Include the standard library.	*/
#endif

#ifndef VMS										/* Include the Unix and DOS headers.	*/
#include <malloc.h>									/* Include memory allocation headers.	*/
#endif

#include "video.h"									/* Include the video headers.		*/
#include "vlocal.h"									/* Include video local headers.		*/
#include "vdata.h"									/* Include the video data base.		*/
#include "vform.h"									/* Include the forms headers.		*/

#include <ctype.h>									/* Get character type macros.		*/
#include <math.h>									/* Get math macros.			*/

#include "vintdef.h"
#include "vplus.h"

/*						Static and Global Data Definitions.						*/

static int active = FALSE;								/* Key activity status.			*/
static int data_valid = FALSE;								/* No data yet.				*/
static int global_keys = FALSE;								/* Assume form level keys.		*/
static unsigned char key_data[8][16];							/* Key data area.			*/
static unsigned char *remember;								/* Remember where it is saved.		*/
static int show_keys();

/*						Subroutine entry point.								*/

void VSETKEYLABEL(comarea,f_or_g,key_num,label) struct vplus_comarea *comarea; int2 *f_or_g; int2 *key_num; char label[];
{
	memcpy(key_data[*key_num],label,16);						/* Copy into the key area.		*/
	data_valid = TRUE;								/* Assume rest of data valid.		*/
}

void VSETKEYLABELS(comarea,f_or_g,num,labels) struct vplus_comarea *comarea; int2 *f_or_g; int2 *num; char labels[];
{
	memcpy(key_data[0], labels, *num * 16);						/* Copy into the key area.		*/
	data_valid = TRUE;								/* Data is now valid.			*/
}

int vkeyset(data) unsigned char *data;							/* Show the help keys.			*/
{
	memcpy(key_data,data,sizeof(key_data));						/* Store the data.			*/
	data_valid = TRUE;								/* Key data is now valid.		*/
	return(SUCCESS);								/* Return to the caller.		*/
}

void vkeyhelp(new_state) int new_state;							/* Turn the help key display on/off.	*/
{
	unsigned char *vsss();								/* Screen save routine.			*/
	register int i,j;								/* Working registers.			*/

	if (!data_valid)								/* Is the data valid?			*/
	{
		for (i = 0; i < 8; i++)							/* Loop through each key.		*/
		{
			for (j = 0; j < 16; j++) key_data[i][j] = ' ';			/* Set to spaces.			*/
		}
		data_valid = TRUE;							/* The data is now valid.		*/
	}

	if (new_state == TOGGLE_KEY_DISPLAY)						/* Toggle states?			*/
	{
		if (active) new_state = OFF;						/* Select the opposite.			*/
		else new_state = ON;
	}

	     if ((new_state == OFF) & !active);						/* Already off and want off.		*/
	else if ((new_state == OFF) &  active) 						/* Turn off, restore what was there.	*/
	{
		vstate(-1);								/* Save the current state.		*/
		vrss(remember);								/* Restore what was there.		*/
		active = FALSE;								/* No longer active.			*/
		vstate(1);								/* Restore the old state.		*/
	}
	else if ((new_state ==  ON) &  active);						/* Already on, show new data.		*/
	else if ((new_state ==  ON) & !active)						/* Turn on, save what was there.	*/
	{
		vstate(-1);								/* Remember the state.			*/
		remember = vsss(MAX_LINES_PER_SCREEN-2,0,2,80);				/* Save what was there.			*/
		show_keys();								/* Now show the keys.			*/
		active = TRUE;								/* Now we're active.			*/
		vstate(1);								/* Restore the state.			*/
	}
}

static int show_keys() 									/* Form keys.				*/
{
	register int i,j;								/* Working registers.			*/
	unsigned char temp[10];								/* Working buffer.			*/

	vbuffering(LOGICAL);								/* Logical section.			*/
	vcharset(DEFAULT);								/* Select the default character set.	*/

	temp[8] = CHAR_NULL;								/* Store an ending null.		*/
	vmode(CLEAR);									/* Select the mode.			*/

	vmove(MAX_LINES_PER_SCREEN-2,0);						/* Move to the key location.		*/
	for (i = 0; i < 8; i++)								/* Loop through all eight keys.		*/
	{
		for (j = 0; j < 8; j++) temp[j] = key_data[i][j];			/* Copy the data.			*/
		space_out(temp);							/* Space it out.			*/
	}
	vputc(' ');
	vmove(MAX_LINES_PER_SCREEN-1,0);						/* Move to the key location.		*/
	for (i = 0; i < 8; i++)								/* Loop through all eight keys.		*/
	{
		for (j = 0; j < 8; j++) temp[j] = key_data[i][j+8];			/* Output the first half.		*/
		space_out(temp);							/* Space it out.			*/
	}
	vputc(' ');
	vbuffering(AUTOMATIC);								/* Auto buffering.			*/
	return(SUCCESS);
}

int space_out(temp) unsigned char *temp;						/* Output the key and space.		*/
{
	if (vcur_col == 36) vprint("        ");						/* In the center space?			*/
	else if (vcur_col < MAX_COLUMNS_PER_LINE-1) vputc(' ');				/* At the edge of the screen?		*/

	vmode(REVERSE);									/* Reverse rendition.			*/
	vprint("%s",temp);								/* Indicate the key.			*/
	vmode(CLEAR);									/* Back to clear.			*/

	return(SUCCESS);
}
