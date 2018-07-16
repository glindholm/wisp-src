static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*	      VIDEO - Video Interactive Development Environment		*/
			/*			Copyright (c) 1988, 1989, 1990			*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/

#include <string.h>

/*						Include standard header files.							*/

#include "video.h"									/* Include video definitions.		*/
#include "vlocal.h"									/* Include local definitions.		*/
#include "vdata.h"									/* Include keyboard control definitions.*/
#include "vcap.h"
#include "vmodules.h"
#include "vraw.h"

static int vchs_do(int char_set);							/* Actually do the selection.		*/


int vcharset(int char_set)								/* Switch to requested character set.	*/
{
	int ret = OPTIMIZED;
	enum e_vop vopt = voptlevel();
	

	if (OFF == vchs_op)
	{
		/*
		**     First time thru - no optimization
		*/
		vopt = VOP_OFF;
		vchs_op = ON;								/* Optimization can now be done		*/
	}

	switch(vopt)
	{
	case VOP_OFF:
	case VOP_TRACKING_ONLY:
	case VOP_DATA_ONLY:
		/*
		**	Controls not optimized
		*/
		ret = vchs_do(char_set);
		break;

	case VOP_DATA_AND_CONTROLS:
	case VOP_DEFER_MOTION_ONLY:
		/*
		**	Simple optimization - if already equal don't do.
		*/
		if (char_set != vchr_set) 
		{
			ret = vchs_do(char_set);
		}
		break;

	case VOP_DEFER_MODE:
		if (char_set != vchr_set) 
		{
			/*
			**	Defer this action
			*/
			vdefer_save();
			vchr_set = char_set;
		}
		break;
	}

	return(ret);
}

static int vchs_do(int char_set)							/* Actually do the selection.		*/
{
	char chstr[MAX_ESC];								/* The control string.			*/

	vdefer_restore();								/* Restore from any held states.	*/
	switch(char_set)								/* Select the character set.		*/
	{
		case VCS_DEFAULT:      {strcpy(chstr,defchs_esc);    break;}		/* Select U.S. or U.K. as appropriate.	*/
		case VCS_GRAPHICS:     {strcpy(chstr,grchs_esc);     break;}		/* Select graphics character set.	*/
		case VCS_ROM_STANDARD: {strcpy(chstr,romstdchs_esc); break;}		/* Select in-ROM character set.		*/
		case VCS_ROM_GRAPHICS: {strcpy(chstr,romgrchs_esc);  break;}		/* Select in-ROM graphics set.		*/
		case VCS_DOWN_LOADED:  {strcpy(chstr,dlldchs_esc);   break;}		/* Select down line loaded font.	*/
		case VCS_US:	  							/* Change default to U.S./Canada.	*/
		{
			strcpy(chstr,uschs_esc); 
			vchs_default = 'B';
			break;
		}
		case VCS_UK:								/* Explicitly select U.K.		*/
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

#ifdef DIRECTVID
	if (vrawdirectio())
	{
		/* Do nothing */
	}
	else
#endif
	{
		vcontrol(chstr);							/* Output the character string.		*/
	}

	vchr_set = char_set;

	return(SUCCESS);								/* And report all went ok.		*/
}
/*
**	History:
**	$Log: vcharset.c,v $
**	Revision 1.10  1997-07-08 16:31:11-04  gsl
**	added vrawdirectio() logic around the vcontrol()
**	Reworked so optimization logic makes sense.
**	Use new video.h defines
**
**	Revision 1.9  1996-10-11 18:16:00-04  gsl
**	drcs update
**
**
**
*/
