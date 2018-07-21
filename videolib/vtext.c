/*
******************************************************************************
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
**
** NOTICE:
** Confidential, unpublished property of Shell Stream Software LLC.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of Shell Stream Software LLC
** is strictly prohibited.
** 
******************************************************************************
*/

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
	
	VL_vbuffering_start();								/* Turn on logical buffering.		*/
	if (display < 0)								/* Old rendition restored on exit?	*/
	{
		display = -display;							/* Convert to positive entity.		*/
		vstate(SAVE);								/* Save information.			*/
		push_flag = TRUE;							/* Flag that we have to restore later.	*/
	}
	else push_flag = FALSE;								/* Flag that we don't have to restore.	*/
	vcharset(VL_vmaskc(display));							/* Select character set.		*/
	VL_vmode(VL_vmaskm(display));								/* Select character rendition.		*/

	vmove(row,column);
	vprint(buffer);									/* Output the text.			*/
	if (push_flag) vstate(RESTORE);							/* Do we have to restore?		*/
	VL_vbuffering_end();								/* Restore automatic buffering.		*/
	va_end(args);
	return(SUCCESS);								/* Return to the caller.		*/
}
/*
**	History:
**	$Log: vtext.c,v $
**	Revision 1.15  2003/01/31 19:25:55  gsl
**	Fix copyright header
**	
**	Revision 1.14  2002/07/17 21:06:05  gsl
**	VL_ globals
**	
**	Revision 1.13  2002/07/15 20:16:15  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.12  1997/07/09 16:31:49  gsl
**	Removed support for double height text
**	
**	Revision 1.11  1996-10-11 18:16:22-04  gsl
**	drcs update
**
**
**
*/
