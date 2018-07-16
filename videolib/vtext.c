static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*									*/
			/*	     VIDEO - Video Interactive Development Environment		*/
			/*									*/
			/*			    Copyright (c) 1987				*/
			/*									*/
			/*	An unpublished work by Greg L. Adams.  All rights reserved.	*/
			/*									*/
			/************************************************************************/


/*						Include standard header files.							*/

#include "video.h"									/* Include video definitions.		*/
#include "vlocal.h"									/* Include local references.		*/
#include "vmodules.h"

#include <stdio.h>
#include <stdarg.h>

/*						Subroutine entry point.								*/

int vtext(int display,int row,int column,char *text,...)
{
	va_list args;
	int push_flag;									/* Flags.				*/
	char buffer[1920];
	
	va_start(args,text);
	vsprintf(buffer,text,args);
	
	vbuffering_start();								/* Turn on logical buffering.		*/
	if (display < 0)								/* Old rendition restored on exit?	*/
	{
		display = -display;							/* Convert to positive entity.		*/
		vstate(SAVE);								/* Save information.			*/
		push_flag = TRUE;							/* Flag that we have to restore later.	*/
	}
	else push_flag = FALSE;								/* Flag that we don't have to restore.	*/
	vcharset(vmaskc(display));							/* Select character set.		*/
	vmode(vmaskm(display));								/* Select character rendition.		*/

	vmove(row,column);
	vprint(buffer);									/* Output the text.			*/
	if (push_flag) vstate(RESTORE);							/* Do we have to restore?		*/
	vbuffering_end();								/* Restore automatic buffering.		*/
	va_end(args);
	return(SUCCESS);								/* Return to the caller.		*/
}
/*
**	History:
**	$Log: vtext.c,v $
**	Revision 1.12  1997/07/09 16:31:49  gsl
**	Removed support for double height text
**	
**	Revision 1.11  1996-10-11 18:16:22-04  gsl
**	drcs update
**
**
**
*/
