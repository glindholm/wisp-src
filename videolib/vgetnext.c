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
#include "vform.h"									/* Define video forms.			*/
#include "vlocal.h"									/* Include internal data.		*/
#include "vdata.h"									/* Include the video database.		*/
static int notimp();

void VGETNEXTFORM(comarea) struct vplus_comarea *comarea;
{
	int i;

	if (comarea->repeatapp == 0) vsetnf(comarea->nfname);				/* Load a new form?			*/

	if ((comarea->repeatapp == 0) || (comarea->repeatapp == 1))			/* Are we freezeable?			*/
	{
		if (comarea->freezapp) freeze_top = highest_line_written;
		else freeze_top = 0;
	}

	if ((comarea->freezapp) || (comarea->repeatapp == 2)) vformcurrent->start_row = highest_line_written + 1;

	if (comarea->repeatapp == 0)
	{
		vputlen(comarea->cfname,vformcurrent->name,HP_FORM_NAME_SIZE);
		comarea->multiusage = 0;  /**** WATCH OUT NOT IMPLEMENTED *****/
		comarea->cfnumlines = vformcurrent->text_rows;
		vputlen(comarea->nfname,vformcurrent->next_form,HP_FORM_NAME_SIZE);
		comarea->repeatapp = vformcurrent->repeatapp;
		comarea->freezapp = vformcurrent->freezapp;
		comarea->dbuflen = vformcurrent->dbuflen;
	}

	comarea->numerrs = 0;
	comarea->cstatus = 0;
	comarea->filerrnum = 0;

	for (i = 0; i < 1920; i++) vform_data_buffer[i] = ' ';				/* Blank out the display buffer.	*/

	return;
}

int vsetnf(nfname) char *nfname;							/* Select the next form			*/
{
	int i,j;									/* Working registers.			*/
	char *ntf;									/* Name to find.			*/
	struct video_form *form;							/* Form data base pointer.		*/

	ntf = nfname;									/* Assume they know where we should go.	*/
	if (veqfn(nfname,"$HEAD           ")) ntf = head_form_name;			/* Asking for the head form.		*/
	if (veqfn(nfname,"$END            ")) notimp("$END");
	if (veqfn(nfname,"$RETURN         ")) notimp("$RETURN");
	if (veqfn(nfname,"$REFRESH        "))
	{
		vdefer(RESTORE);
		vrefresh(HARD_REFRESH);
		return(SUCCESS);
	}

	form = vformdata;								/* Point to the first form.		*/
	for (i = 0, j = -1; ((i < form_count) && (j < 0)); i++)				/* Loop and find the name.		*/
	{
		if (veqfn(ntf,form->name)) j = i;
		if (i != j) form++;
	}

	if (j < 0)
	{
		char temp[24];
		vtrimlen(temp,ntf,HP_FORM_NAME_SIZE);
		verase(FULL_SCREEN);
		vre_window("vsetnf: Could not find form %s",temp);
		vexit(0); exit(0);
	}

	current_form = j;
	vformcurrent = form;
}

static int notimp(what) char *what;
{
	verase(FULL_SCREEN);
	vre_window("vsetnf: %s not implemented.",what);
	vexit(0); exit(0);
}

int veqfn(n0,n1) char n0[],n1[];				/* Find out if two form names are the same.		*/
{
	register int i,j;

	j = TRUE;
	for (i = 0; i < HP_FORM_NAME_SIZE; i++)
	{
		if (n0[i] != n1[i]) j = FALSE;
	}
	return(j);
}
