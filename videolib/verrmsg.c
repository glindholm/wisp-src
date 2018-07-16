			/************************************************************************/
			/*	     VIDEO - Video Interactive Development Environment		*/
			/*			Copyright (c) 1987-1993				*/
			/*	An unpublished work by International Digital Scientific Inc.	*/
			/*			  All rights reserved.				*/
			/************************************************************************/

#include <stdio.h>									/* Include header files.		*/
#include "video.h"									/* Include the video database.		*/
#include "vform.h"									/* Include the view database.		*/
#include "vlocal.h"
#include "vintdef.h"
#include "vplus.h"
#include "vdata.h"

static char local_error_message[82];							/* Local error message buffer.		*/
static int get_line();

/*						Subroutine entry point.								*/

void VSETERROR(comarea,fieldnum,message,msglen) struct vplus_comarea *comarea; int2 *fieldnum; char *message; int2 *msglen;
{
	int i,j;									/* Working integers.			*/
	int f;

	for (i = 0, f = -1; (i < vformcurrent->fields) && (f == -1); i++)		/* Find the field number.		*/
	{
		if (vformcurrent->field[i].number == *fieldnum) f = i;			/* Field is found.			*/ 
	}

	if (f == -1)									/* No such field number.		*/
	{
		comarea->cstatus = -1;							/* Flag an error.			*/
		return;									/* Return to the caller.		*/
	}

	comarea->numerrs++;								/* Count the error.			*/
	comarea->cstatus = LOCAL_ERROR_ACTIVE;						/* No error.				*/
	vformcurrent->field[f].error_code = LOCAL_ERROR_ACTIVE;				/* Local error is active.		*/
	vputlocal_error_message(message,(int)*msglen);					/* Output the message.			*/
}

void VERRMSG(comarea,buffer,buflen,actualen) struct vplus_comarea *comarea; char *buffer; int2 *buflen, *actualen;
{
	int i,j;
	FILE *fid, *fopen();
	char inbuf[256];
	int found;
	char	*vinfoname();
	char	*forms_error_file;

	if (comarea->cstatus == LOCAL_ERROR_ACTIVE)					/* Is this a local error?		*/
	{
		vputlen(buffer,local_error_message,72);					/* Send the message.			*/
		*actualen = 72;								/* Say it was 72 characters.		*/
		return;									/* Return to the caller.		*/
	}

	forms_error_file = vinfoname(FORMS_ERROR_FILE);					/* Construct int full name		*/
	fid = fopen(forms_error_file,"r");						/* Open the forms error file.		*/
	if (fid == NULL)
	{
		vre_window("VERRMSG: Unable to find forms error file %s",forms_error_file);
		vexit(0); exit(0);
	}

	found = FALSE;									/* Assume error message not found.	*/
	while (!found && get_line(fid,inbuf))						/* Repeat until we fail.		*/
	{
		sscanf(inbuf,"%d",&i);							/* Get the error number.		*/
		if (i == comarea->cstatus) found = TRUE;				/* Is this it?				*/
	}

	fclose(fid);
	if (!found) sprintf(&inbuf[4],"Unable to find error number %d in the message file.", comarea->cstatus);
	vputlen(buffer,&inbuf[4],72);							/* Send the message.			*/
	*actualen = 72;									/* Say it was 72 characters.		*/
}

int vputlocal_error_message(message,size) char *message; int size;			/* Put a local error message.		*/
{
	int i;

	for (i = 0; i < 80; i++) local_error_message[i] = ' ';

	vputlen(local_error_message,message,size);					/* Store the message.			*/
}

static int get_line(in, buffer) FILE *in; unsigned char *buffer;
{
	int i;
	unsigned char *bufptr;

	bufptr = buffer;
	while ((*bufptr = getc(in)) != EOF)							/* Repeat until end of file.	*/
	{
		if ((*bufptr == '\n') || (*bufptr == '\014'))					/* Is this end of line?		*/
		{
			*bufptr = CHAR_NULL;							/* Store a null.		*/
			return(SUCCESS);
		}
		else bufptr++;									/* And don't go beyond the end.	*/
	}
	return(FAILURE);
}

