/*
******************************************************************************
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** $Id:$
**
** NOTICE:
** Confidential, unpublished property of NeoMedia Technologies, Inc.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of NeoMedia 
** Technologies, Inc. is strictly prohibited.
** 
** CVS
** $Source:$
** $Author: gsl $
** $Date:$
** $Revision:$
******************************************************************************
*/

			/************************************************************************/
			/*	     VIDEO - Video Interactive Development Environment		*/
			/*			Copyright (c) 1987-1991				*/
			/*	An unpublished work by International Digital Scientific Inc.	*/
			/*			  All rights reserved.				*/
			/************************************************************************/

/*						Keystroke Macro Manager								*/

#include "video.h"
#include "vlocal.h"
#include "vdata.h"
#include "vmodules.h"

/*						Subroutine entry point.								*/

int VL_vmacro(action) int action;
{
	int ret = FAILURE;									/* Subroutine return code.		*/

	switch (action)									/* Select the action.			*/
	{
		case START_SAVE:							/* Start saving a macro?		*/
		{
			if ((VL_macro_output == NULL) && (VL_macro_input == NULL))		/* Don't save is already doing so.	*/
			{
				if ((VL_macro_output = VL_vopenf("mac","w+")) == NULL)		/* Did it work?				*/
				{
					vre("vmacro(START_SAVE)-W-ERRONMACFILE Error opening macro save file.");
					ret = FAILURE;
				}
				else ret = SUCCESS;					/* Yes, so all ok.			*/
			}
			else ret = FAILURE;						/* Did not do the request.		*/
			break;								/* Nothing more to do.			*/
		}

		case END_SAVE:								/* Terminate saving a macro.		*/
		{
			if (VL_macro_output != NULL)					/* Don't close if not saving.		*/
			{
				fclose(VL_macro_output);					/* Close the file.			*/
				VL_macro_output = NULL;					/* Null out the pointer.		*/
				ret = SUCCESS;						/* Return success.			*/
			}
			else ret = FAILURE;						/* Oops, they weren't saving one.	*/
			break;
		}

		case START_RESTORE:							/* Input a keystroke macro.		*/
		{
			if ((VL_macro_input == NULL) && (VL_macro_output == NULL))		/* Are we already processing one?	*/
			{
				if ((VL_macro_input = VL_vopenf("mac","r")) == NULL)		/* Open the macro input file.		*/
				{
					ret = FAILURE;					/* Didn't find one.			*/
				}
				else ret = SUCCESS;					/* We are now processing a macro.	*/
			}
			else ret = FAILURE;						/* Oops.				*/
			break;
		}
	}
	return(ret);									/* Return to the caller.		*/
}
/*
**	History:
**	$Log: vmacro.c,v $
**	Revision 1.14  2003/01/31 20:58:40  gsl
**	Fix -Wall warnings
**	
**	Revision 1.13  2003/01/31 19:25:56  gsl
**	Fix copyright header
**	
**	Revision 1.12  2002/07/15 20:56:39  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.11  2002/07/15 20:16:10  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.10  2002/07/15 17:10:04  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.9  1996/10/11 22:16:10  gsl
**	drcs update
**	
**
**
*/
