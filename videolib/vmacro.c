static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
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

FILE *vopenf();

/*						Subroutine entry point.								*/

vmacro(action) int action;
{
	int ret;									/* Subroutine return code.		*/

	switch (action)									/* Select the action.			*/
	{
		case START_SAVE:							/* Start saving a macro?		*/
		{
			if ((macro_output == NULL) && (macro_input == NULL))		/* Don't save is already doing so.	*/
			{
				if ((macro_output = vopenf("mac","w+")) == NULL)		/* Did it work?				*/
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
			if (macro_output != NULL)					/* Don't close if not saving.		*/
			{
				fclose(macro_output);					/* Close the file.			*/
				macro_output = NULL;					/* Null out the pointer.		*/
				ret = SUCCESS;						/* Return success.			*/
			}
			else ret = FAILURE;						/* Oops, they weren't saving one.	*/
			break;
		}

		case START_RESTORE:							/* Input a keystroke macro.		*/
		{
			if ((macro_input == NULL) && (macro_output == NULL))		/* Are we already processing one?	*/
			{
				if ((macro_input = vopenf("mac","r")) == NULL)		/* Open the macro input file.		*/
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
**	Revision 1.9  1996/10/11 22:16:10  gsl
**	drcs update
**	
**
**
*/
