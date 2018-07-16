			/************************************************************************/
			/*	      VIDEO - Video Interactive Development Environment		*/
			/*			Copyright (c) 1988, 1989, 1990			*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/

/*						Include standard header files.							*/

#include "video.h"									/* Include video definitions.		*/
#include "vlocal.h"									/* Include local definitions.		*/
#include "vdata.h"									/* Include keyboard control definitions.*/
#include "vcap.h"

static int vchs_do();

/*						Subroutine entry point.								*/

int vcharset(char_set) int char_set;							/* Switch to requested character set.	*/
{
	register int ret;								/* Working register.			*/

	ret = OPTIMIZED;								/* Assume we will be optimized.		*/

	if ((!vchs_op) || (optimization <= DATA_ONLY))					/* Should we optimize?			*/
	{
		vchs_do(char_set);							/* No, so set the selection.		*/
		vchs_op = TRUE;								/* No more local optimization.		*/
	}

	else if (optimization <= DATA_CONTROLS_AND_MOTION)				/* Optimize controls?			*/
	{
		if (char_set != vchr_set) ret = vchs_do(char_set);			/* Select if not already done.		*/
	}

	else if (char_set != vchr_set) vdefer(SAVE);					/* Else we must defer this action.	*/

	vchr_set = char_set;								/* Now remember this change.		*/
	return(ret);									/* Return to the caller.		*/
}

static int vchs_do(char_set) int char_set;						/* Actually do the selection.		*/
{
	char chstr[MAX_ESC];								/* The control string.			*/
	register int i;									/* Working register.			*/

	vdefer(RESTORE);								/* Restore from any held states.	*/
	switch(char_set)								/* Select the character set.		*/
	{
		case DEFAULT:      {strcpy(chstr,defchs_esc); break;}			/* Select U.S. or U.K. as appropriate.	*/
		case GRAPHICS:     {strcpy(chstr,grchs_esc); break;}			/* Select graphics character set.	*/
		case ROM_STANDARD: {strcpy(chstr,romstdchs_esc); break;}		/* Select in-ROM character set.		*/
		case ROM_GRAPHICS: {strcpy(chstr,romgrchs_esc); break;}			/* Select in-ROM graphics set.		*/

		case DOWN_LOADED: {strcpy(chstr,dlldchs_esc); break;}			/* Select down line loaded font.	*/
		case US:	  							/* Change default to U.S./Canada.	*/
		{
			strcpy(chstr,uschs_esc); 
			vchs_default = 'B';
			break;
		}
		case UK:								/* Explicitly select U.K.		*/
		{
			strcpy(chstr,ukchs_esc);
			vchs_default = 'A';
			break;
		}

		default:
		{
			vre("vcharset(%d)-Invalid character set code",char_set);
			return(FAILURE);						/* Oops, invalid character set.		*/
		}
	}

	vcontrol(chstr);								/* Output the character string.		*/
	return(SUCCESS);								/* And report all went ok.		*/
}
