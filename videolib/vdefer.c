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


/*
This seems to revolve around 2 variables, "optimization", and "deferred".

I think:
The optimization level is the "requested" level of optimization.
While "deferred" is what is being deferred (held until later).

The "true" line and col (VL_tcur_lin, VL_tcur_col) is where the cursor really is
while the vcur_lin and vcur_col is the logical position. The "true" values
are only valid when motion (vmove) has been deferred (VOP_DEFER_MOTION_ONLY 
or greater).

VOP_OFF				= 0 No optimization and no tracking of current state.
                                    All requested actions are done immediately.

VOP_TRACKING_ONLY 		= 1 Only track output, no optimization.
VOP_DATA_ONLY	  		= 2 Data only optimization.
VOP_DATA_AND_CONTROLS 		= 3 Optimize data and controls.

VOP_DEFER_MOTION_ONLY 		= 4 Defer movement and optimize data and controls.
VOP_DEFER_MODE	  		= 5 Defer all actions

VDEFER_SAVE			= -1, 
VDEFER_OFF			= 0 Nothing currently deferred
VDEFER_RESTORE			= 1 Everything that can be is deferred
VDEFER_MOTION_ONLY		= 2 Motion (vmove()) has been deferred

*/

/*					Include standard header files.								*/

#include "video.h"									/* Include video definitions.		*/
#include "vlocal.h"									/* Include internal definitions.	*/
#include "vintdef.h"
#include "vdata.h"
#include "vmodules.h"


/*		Data involved in deferred optimization (captured the first time deferred action enabled).			*/

static int tchr_set = VCS_DEFAULT;							/* True character set.			*/
static int tcur_atr = VMODE_CLEAR;							/* True character rendition.		*/
static int tcur_set[VSET_TABLE_SIZE] = {NORMAL,OFF,VISIBLE};				/* True settings (during defer).	*/
static int trol_top = 0;								/* Top of current scrolling area.	*/
static int trol_bot = MAX_LINES_PER_SCREEN-1;						/* Bottom of current scrolling area.	*/

static enum e_vop optimization = VOP_DEFER_MODE;		       		/* Master optimization starts in defered mode.	*/
static enum e_vdefer deferred = VDEFER_OFF;					/* Assume actions are not deferred.		*/


/*					Subroutine entry point.									*/

enum e_vdefer VL_vdeferred(void)
{
	return deferred;
}

int VL_vdefer_restore(void)
{
	return vdefer(VDEFER_RESTORE);
}
int VL_vdefer_save(void)
{
	return vdefer(VDEFER_SAVE);
}


int VL_vdefer(enum e_vdefer state)								/* State is one of SAVE or RESTORE.	*/
{
	register int i, t0, t1, ret;							/* Some working registers.		*/
	enum e_vop op_save;								/* Location to save optimization.	*/

	switch (state)
	{
		case VDEFER_OFF:
		{
			/*
			**	Set deferred to VDEFER_OFF and loose all deferred actions
			*/
			deferred = VDEFER_OFF;
			ret = SUCCESS;							/* Flag success.			*/
			break;								/* Return to the caller.		*/
		}

		case VDEFER_MOTION_ONLY:						/* Save position information only.	*/
		{
			if (VDEFER_OFF == deferred)					/* Are we already deferred?		*/
			{
				VL_tcur_lin = vcur_lin;					/* Save where we are.			*/
				VL_tcur_col = vcur_col;
				deferred = VDEFER_MOTION_ONLY;				/* Flag that we have motion deferred.	*/
			}
			ret = SUCCESS;							/* Flag success.			*/
			break;								/* Return to the caller.		*/
		}

		case VDEFER_SAVE:							/* Save for deferred action.		*/
		{
			if (optimization < VOP_DEFER_MODE)				/* Is it valid to defer?		*/
			{
				vre("vdefer(SAVE)-Internal error, attempt to defer when optimization < VOP_DEFER_MODE");
			}
			if ((deferred ==  VDEFER_OFF) || (deferred == VDEFER_MOTION_ONLY)) /* Don't do it if already deferred.	*/
			{
				if (deferred == VDEFER_OFF)				/* Is position already saved?		*/
				{
					VL_tcur_lin = vcur_lin;				/* Yes, then remember where we are.	*/
					VL_tcur_col = vcur_col;				/* Remember the column too.		*/
				}
				tcur_atr = vcur_atr;					/* Remember the character rendition.	*/
				tchr_set = vchr_set;					/* Remember the character set.		*/
				VL_imemcpy(tcur_set,vcur_set,VSET_TABLE_SIZE);		/* Remember the Terminal setup.		*/
				trol_top = vrol_top;					/* Remember the scrolling region.	*/
				trol_bot = vrol_bot;
				deferred = VDEFER_RESTORE;				/* And now we're deferred.		*/
			}
			ret = SUCCESS;							/* All is ok.				*/
			break;
		}

		case VDEFER_RESTORE:
		{
			ret = SUCCESS;							/* Assume all is well.			*/

			VL_vbuffering_start();					/* Turn on logical buffering.		*/

			if (deferred == VDEFER_MOTION_ONLY)				/* Are we just deferring motion.	*/
			{
				op_save = optimization;					/* Save the current optimization.	*/
				optimization = VOP_DATA_AND_CONTROLS;			/* Reduct to data only level.		*/
				deferred = VDEFER_OFF;					/* Now we're not deferred.		*/
				t0 = vcur_lin;						/* Load temporary.			*/
				t1 = vcur_col;
				vcur_lin = VL_tcur_lin;					/* Where we really are.			*/
				vcur_col = VL_tcur_col;
				vmove(t0,t1);						/* Move to the requested location.	*/
				optimization = op_save;					/* Restore current optimization.	*/
			}

			if (deferred == VDEFER_RESTORE)					/* Deferred?				*/
			{
				op_save = optimization;					/* Save the current optimization.	*/
				optimization = VOP_DATA_AND_CONTROLS;			/* Reduct to data only level.		*/
				deferred = VDEFER_OFF;					/* Now we're not deferring.		*/

				for (i = 0; i < VSET_TABLE_SIZE; i++)			/* Restore the terminal setup.		*/
				{
					t0 = vcur_set[i];				/* Load temporary value.		*/
					vcur_set[i] = tcur_set[i];			/* Set current to what it really is.	*/
					vset(i,t0);					/* Now restore the real setup.		*/
				}

				t0 = vcur_atr;						/* Load temporary.			*/
				vcur_atr = tcur_atr;					/* What they really are.		*/
				VL_vmode(t0);						/* Now restore the real rendition.	*/

				t0 = vchr_set;						/* Load temporary.			*/
				vchr_set = tchr_set;					/* What it really is.			*/
				vcharset(t0);						/* Restore the real character set.	*/

				t0 = vrol_top;						/* Load temporary.			*/
				t1 = vrol_bot;
				vrol_top = trol_top;					/* Restore scrolling area.		*/
				vrol_bot = trol_bot;
				vroll(t0,t1);						/* Now set scrolling region.		*/

				t0 = vcur_lin;						/* Load temporary.			*/
				t1 = vcur_col;
				vcur_lin = VL_tcur_lin;					/* Where we really are.			*/
				vcur_col = VL_tcur_col;
				vmove(t0,t1);						/* Move to the requested location.	*/

				optimization = op_save;					/* Restore current optimization.	*/
			}
			VL_vbuffering_end();						/* Restore automatic buffering.		*/
			break;								/* All done.				*/
		}

		default:
		{
			vre("vdefer(%d)-Invalid parameter (not SAVE, RESTORE or PURGE",state);		/* Report errors.	*/
			ret = FAILURE;									/* Return failure.	*/
		}
	}

	return(ret);									/* Return to the caller.		*/
}


/*						Subroutine entry point.								*/

enum e_vop voptimize(enum e_vop new_op)
{
	enum e_vop old_op;

	old_op = optimization;						/* Remember the old optimization.	*/
	vdefer_restore();						/* No, so restore from deferred action.	*/
	optimization = new_op;						/* Select the optimization.		*/

	return old_op;
}

enum e_vop voptlevel(void)
{
	return optimization;
}


/*
**	History:
**	$Log: vdefer.c,v $
**	Revision 1.16  2003/01/31 19:25:56  gsl
**	Fix copyright header
**	
**	Revision 1.15  2002/07/17 21:06:01  gsl
**	VL_ globals
**	
**	Revision 1.14  2002/07/15 20:16:08  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.13  2002/07/15 17:10:03  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.12  1997/07/12 21:32:57  gsl
**	Make deferred variable local
**	
**	Revision 1.11  1997-07-08 16:34:22-04  gsl
**	Removed the BLOCK_MODE deferred and optimization.
**	Localized all the optimization and deferred variables.
**	Use new video.h defines
**	Documented the interaction of optimization and deferred modes
**
**	Revision 1.10  1997-05-21 14:11:00-04  gsl
**	Change to use VDEFER_xxx defines
**
**	Revision 1.9  1996-10-11 18:16:02-04  gsl
**	drcs update
**
**
**
*/
