			/************************************************************************/
			/*									*/
			/*	     VIDEO - Video Interactive Development Environment		*/
			/*									*/
			/*			    Copyright (c) 1987				*/
			/*									*/
			/*	An unpublished work by Greg L. Adams.  All rights reserved.	*/
			/*									*/
			/************************************************************************/


/*					Include standard header files.								*/

#include "video.h"									/* Include video definitions.		*/
#include "vlocal.h"									/* Include internal definitions.	*/

/*					Subroutine entry point.									*/

int vdefer(state) int state;								/* State is one of SAVE or RESTORE.	*/
{
	extern int vcur_col, vcur_lin, tcur_lin, tcur_col;				/* Row and column position parameters.	*/
	extern int vcur_atr, tcur_atr;							/* Attribute control.			*/
	extern int vchr_set, tchr_set;							/* Character set control.		*/
	extern int vcur_set[], tcur_set[];						/* vset control tables.			*/
	extern int vlin_atr[], tlin_atr[];						/* Line size control tables.		*/
	extern int vrol_top, vrol_bot, trol_top, trol_bot;				/* Scroll area data.			*/
	extern int optimization, deferred;						/* Deferred action control flags.	*/
	register int i, t0, t1, ret;							/* Some working registers.		*/
	int op_save;									/* Location to save optimization.	*/

	switch (state)
	{
		case MOTION_ONLY:							/* Save position information only.	*/
		{
			if (!deferred)							/* Are we already deferred?		*/
			{
				tcur_lin = vcur_lin;					/* Save where we are.			*/
				tcur_col = vcur_col;
				deferred = MOTION_ONLY;					/* Flag that we have motion deferred.	*/
			}
			ret = SUCCESS;							/* Flag success.			*/
			break;								/* Return to the caller.		*/
		}

		case SAVE:								/* Save for deferred action.		*/
		{
			if (optimization < DEFER_MODE)					/* Is it valid to defer?		*/
			{
				vre("vdefer(SAVE)-Internal error, attempt to defer when optimization < DEFER_MODE");
			}
			if ((deferred ==  OFF) || (deferred == MOTION_ONLY))		/* Don't do it if already deferred.	*/
			{
				if (deferred == OFF)					/* Is position already saved?		*/
				{
					tcur_lin = vcur_lin;				/* Yes, then remember where we are.	*/
					tcur_col = vcur_col;				/* Remember the column too.		*/
				}
				tcur_atr = vcur_atr;					/* Remember the character rendition.	*/
				tchr_set = vchr_set;					/* Remember the character set.		*/
				imemcpy(tcur_set,vcur_set,INT_SET_TABLE_SIZE);		/* Remember the Terminal setup.		*/
				imemcpy(tlin_atr,vlin_atr,MAX_LINES_PER_SCREEN);	/* Remember the line attributes.	*/
				trol_top = vrol_top;					/* Remember the scrolling region.	*/
				trol_bot = vrol_bot;
				deferred = TRUE;					/* And now we're deferred.		*/
			}
			ret = SUCCESS;							/* All is ok.				*/
			break;
		}

		case RESTORE:
		{
			ret = SUCCESS;							/* Assume all is well.			*/

			vbuffering(LOGICAL);						/* Turn on logical buffering.		*/

			if (deferred == MOTION_ONLY)					/* Are we just deferring motion.	*/
			{
				op_save = optimization;					/* Save the current optimization.	*/
				optimization = DATA_AND_CONTROLS;			/* Reduct to data only level.		*/
				deferred = FALSE;					/* Now we're not deferred.		*/
				t0 = vcur_lin;						/* Load temporary.			*/
				t1 = vcur_col;
				vcur_lin = tcur_lin;					/* Where we really are.			*/
				vcur_col = tcur_col;
				vmove(t0,t1);						/* Move to the requested location.	*/
				optimization = op_save;					/* Restore current optimization.	*/
			}

			if (deferred == BLOCK_MODE)
			{
				op_save = optimization;					/* Remember the optimization.		*/
				optimization = OFF;					/* Now turn of the optimization.	*/
				deferred = OFF;						/* Pretend not deferred for a moment.	*/
				vrefresh(FULL_SCREEN);					/* Refresh screen from the map.		*/
				optimization = op_save;					/* Restore current optimization.	*/
				deferred = ON;						/* Reduce deferred action to just on.	*/
			}

			if (deferred == ON)						/* Deferred?				*/
			{
				op_save = optimization;					/* Save the current optimization.	*/
				optimization = DATA_AND_CONTROLS;			/* Reduct to data only level.		*/
				deferred = OFF;						/* Now we're not deferring.		*/

				for (i = 0; i < INT_SET_TABLE_SIZE; i++)		/* Restore the terminal setup.		*/
				{
					t0 = vcur_set[i];				/* Load temporary value.		*/
					vcur_set[i] = tcur_set[i];			/* Set current to what it really is.	*/
					vset(i,t0);					/* Now restore the real setup.		*/
				}

				t0 = vcur_atr;						/* Load temporary.			*/
				vcur_atr = tcur_atr;					/* What they really are.		*/
				vmode(t0);						/* Now restore the real rendition.	*/

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
				vcur_lin = tcur_lin;					/* Where we really are.			*/
				vcur_col = tcur_col;
				vmove(t0,t1);						/* Move to the requested location.	*/

				for (i = 0; i < MAX_LINES_PER_SCREEN; i++)
				{
					t0 = vlin_atr[i];				/* Load temporary.			*/
					vlin_atr[i] = tlin_atr[i];			/* Restore the line setup.		*/
					vsize(i,t0);					/* And action it.			*/
				}

				optimization = op_save;					/* Restore current optimization.	*/
			}
			vbuffering(AUTOMATIC);						/* Restore automatic buffering.		*/
			break;								/* All done.				*/
		}

		case PURGE:								/* Terminate without resolution.	*/
		{
			if (deferred)							/* Unless action is deferred there is	*/
			{								/*   nothing to do.			*/
				deferred = FALSE;					/* Turn of deferred action flag.	*/
				vcur_lin = tcur_lin;					/* Restore the line number.		*/
				vcur_col = tcur_col;					/* Restore the current column.		*/
				vcur_atr = tcur_atr;					/* Restore character rendition.		*/
				vchr_set = tchr_set;					/* Restore old character set.		*/
				imemcpy(vcur_set,tcur_set,INT_SET_TABLE_SIZE);		/* Restore old terminal setup.		*/
				imemcpy(vlin_atr,tlin_atr,MAX_LINES_PER_SCREEN);	/* Restore old line attributes.		*/
				vrol_top = trol_top;					/* Restore scrolling area.		*/
				vrol_bot = trol_bot;
			}
			ret = SUCCESS;							/* All is well.				*/
			break;								/* And exit.				*/
		}

		default:
		{
			vre("vdefer(%d)-Invalid parameter (not SAVE, RESTORE or PURGE",state);		/* Report errors.	*/
			ret = FAILURE;									/* Return failure.	*/
		}
	}

	return(ret);									/* Return to the caller.		*/
}
