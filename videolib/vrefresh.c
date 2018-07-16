static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
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
#include "vmodules.h"

#define STATE_SAVE	-1
#define STATE_RESTORE	1

/*						Static data definitions.							*/

static int save_lin, save_col, save_atr, save_chs;					/* Save locations.			*/
static int state(int action);
static int vre_hard();

/*						Subroutine entry point.								*/

int vrefresh(int what)									/* Refresh screen from maps.		*/
{
	register int ret;								/* Return code.				*/

	switch (what)									/* What are we to do.			*/
	{
		/* case FULL_SCREEN:  {ret = vre_full(); break;} */			/* Do a regular refresh.		*/
		case HARD_REFRESH: {ret = vre_hard(); break;}				/* Do a hard refresh.			*/
		default:
		{
			vre("vrefresh(%d)-Invalid parameter (or not implemented).");	/* Report what has happened.		*/
			ret = FAILURE;							/* And return failure to the caller.	*/
		}
	}

	return(ret);									/* Return to the caller.		*/
}

/*						Subroutine to do hard refreshes.						*/

static int vre_hard()
{
	enum e_vop old_op;								/* Save locations.			*/
	register int i, j, k;								/* Working registers.			*/
	register char c;								/* Working characters.			*/

	if (vdeferred() != VDEFER_OFF)							/* Are we in deferred mode?		*/
	{
		vre("vrefresh(HARD_REFRESH)-Internal error, cannot hard refresh while in deferred mode.");
		return(FAILURE);							/* Don't let things go past here.	*/
	}
	
	state(STATE_SAVE);								/* Save everything.			*/
	old_op = voptimize(VOP_OFF);							/* Now we don't want any optimization.	*/
	vbuffering(VBUFF_START);							/* Turn on logical buffering.		*/

	vmove(0,0);									/* Avoid cursor dashing.		*/
	for (i = 0; i < VSET_TABLE_SIZE; i++) vset(i,vcur_set[i]);			/* Restore the current settings.	*/
	vscreen(vscr_atr);								/* Reset screen, note may clear screen.	*/
	verase(FULL_SCREEN);

	vmov_op = OFF;									/* Force no optimization 1st time.	*/
	vchs_op = OFF;
	vmod_op = OFF;

	vdefer_restore();
	voptimize(VOP_DATA_AND_CONTROLS);
	vbuffering(VBUFF_END);

	vbuffering(VBUFF_START);

	for (i = 0; i < MAX_LINES_PER_SCREEN; i++)					/* Yes so loop through every line.	*/
	{
		k = vml(i);								/* Resolve to table location.		*/
		for (j = 0; j < vedge(i); j++)						/* Loop through every column.		*/
		{
			if (vmap_cng[k][j] < 0)						/* Is this old data?			*/
			{
				vmap_cng[k][j] = 0;					/* Yes, then just erase from maps.	*/
				vatr_map[k][j] = 0;
				vchr_map[k][j] = ' ';
			}
			if (((i == 0) && (j == 0)) || visible(vchr_map[k][j],vatr_map[k][j]))		/* No, is this visible?	*/
			{
				vmove(i,j);						/* Yes so move to it.			*/
				c = vchr_map[k][j];					/* Get the character in the map.	*/
				if (vmap_cng[k][j] < 0) 				/* Is this old data?			*/
				{
					c = ' ';					/* Yes character should be space.	*/
					vatr_map[k][j] = 0;				/* And attributes are no longer valid.	*/
				}
				vcharset(vmaskc(vatr_map[k][j]));			/* Select the character set.		*/
				vmode(vmaskm(vatr_map[k][j]));				/* Select the rendition.		*/
				vchr_map[k][j] = 0;					/* Force vprint to output it.		*/
				vputc(c);						/* Output the character.		*/
			}
		}
	}

	vdefer_restore();								/* Cannot leave in deferred mode.	*/
	voptimize(VOP_TRACKING_ONLY);							/* Turn optimization off again.		*/
	vroll(vrol_top,vrol_bot);							/* Reset the scrolling region.		*/
	state(STATE_RESTORE);								/* Restore where we were.		*/
	voptimize(old_op);								/* Put optimization back on too.	*/
	vbuffering(VBUFF_END);								/* Restore automatic buffering.		*/
	return(SUCCESS);								/* And we're done.			*/
}


static int state(int action)								/* Perform specified action.		*/
{
	switch(action)									/* Select the action.			*/
	{
		case STATE_SAVE:							/* Save current setup and status.	*/
		{
			save_lin = vcur_lin;						/* Save position.			*/
			save_col = vcur_col;
			save_atr = vcur_atr;						/* Save character rendition.		*/
			save_chs = vchr_set;						/* Save character set.			*/
			break;
		}

		case STATE_RESTORE:							/* Restore old setup and status.	*/
		{
			vmode(save_atr);						/* Restore old character rendition.	*/
			vcharset(save_chs);						/* Restore character set.		*/
			vmove(save_lin,save_col);					/* Restore old position.		*/
			break;
		}
	}
	return 0;
}

/*
**	History:
**	$Log: vrefresh.c,v $
**	Revision 1.15  1997-07-09 12:08:25-04  gsl
**	Change to use new video.h interfaces
**
**	Revision 1.14  1997-05-20 10:33:44-04  gsl
**	In vre_hard() change optimization to TRACKING_ONLY for performance
**
**	Revision 1.13  1997-05-19 17:03:40-04  gsl
**	On a vre_hard() the repaint was painfully slow on WIN32.
**	Turn the optimization back on after eraseing the full screen
**
**	Revision 1.12  1997-01-11 15:47:31-05  gsl
**	Removed the vrefresh(FULL_SCREEN) logic as it was never used and it
**	complicated the vraw erase logic.
**	Added a verase(FULL_SCREEN) in vre_hard() following the vscreen() call
**	because vscreen() has been corrected and may not erase the screen.
**
**	Revision 1.11  1996-10-11 15:16:18-07  gsl
**	drcs update
**
**
**
*/
