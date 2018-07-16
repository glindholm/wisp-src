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

/*						Subroutine entry point.								*/

int vnewline(direction)									/* Generate a CR/LF.			*/
	int direction;
{
	return(vcrlfx(direction,TRUE));							/* Call CR/LF execute routine.		*/
}


int vlinefeed(direction)								/* Generate a linefeed			*/
	int direction;
{
	return(vcrlfx(direction,FALSE));						/* Call Video CR/LF eXecute routine.	*/
}


int vcrlfx(direction,do_cr)								/* Move up or down a line.		*/
	int direction,do_cr;								/* Is it up or down? With a CR too?	*/
{
	register int ret, i;								/* Return status.			*/
	int save_pure;									/* Location to save CR/LF pure state.	*/

	switch(direction)
	{
		case FORWARD:								/* Newline or LF in forward direction.	*/
		{
			save_pure = vb_pure;						/* Remember the I/O pure mode.		*/
			if (do_cr)  vb_pure = FALSE;					/* Linefeeds generate new lines.	*/
			else 	    vb_pure = TRUE;					/* Let linefeeds go through unscathed.	*/
			vprint("\n");							/* Send one through now.		*/
			vb_pure = save_pure;						/* Set purity back to what it was.	*/
			break;								/* We're all done.			*/
		}

		case REVERSE:								/* Newline or LF in reverse direction.	*/
		{
			int noscroll_avail, svo, cmpos;
			char temp[MAX_COLUMNS_PER_LINE+1];

			vbuffering(LOGICAL);						/* Logical buffering.			*/
			vdefer(RESTORE);						/* Must restore from deferred mode.	*/
#ifdef MSDOS
			if ( (0 == vcur_lin) || (vrol_top == vcur_lin) )		/* Top of page or scroll region?	*/
			{
				vrawmove(vcur_lin,vcur_col);				/* Set row and column.			*/
				vrawscroll( -1 );					/* Scroll down.				*/
			}
			else
			{
				vrawmove((vcur_lin - 1), vcur_col);			/* Set new row and column.		*/
			}
#else	/* VMS or unix */
			if (!(noscroll_avail = vcapnull(rvrsidx_esc, "SCROLL_REVERSE",0))) /* test if defined.			*/
			{								/* YES! it is defined.			*/
				vcontrol(rvrsidx_esc);					/* Send out reverse index command.	*/
				if (do_cr)						/* A newline format?			*/
				{
					vcontrol("\015");				/* Add a CR if needed.			*/
					vcur_col = 0;					/* Record occurence of a CR		*/
				}
			}
#endif	/* VMS or unix */
			vcur_lin--;							/* up 1 line				*/
			if (vcur_lin < 0)						/* Did we hit the top of screen?	*/
			{
				vcur_lin = 0;						/* Set current line = top of screen     */
				vdefer(RESTORE);					/* Make sure screen is updated		*/
				vmap(SCROLL_DOWN,0,0,MAX_LINES_PER_SCREEN-1,0);		/* Scroll whole screen			*/
				if (noscroll_avail)
				{
					svo = voptimize(OFF);				/* Turn optimization OFF.		*/
					cmpos = vmap_top;
					for (i = MAX_LINES_PER_SCREEN-1; i >= 0 ; i--)
					{
						vmove(i,0);
						if (cmpos == 0) cmpos = MAX_LINES_PER_SCREEN;
						memcpy(temp,vchr_map[cmpos-1],vscr_wid);
						temp[vscr_wid] = '\0';
						vprint("%s",temp);
						cmpos--;
					}
					vmove(0,0);
					voptimize(svo);					/* Set back to what it was.		*/
				}
			}
			else if (vcur_lin == (vrol_top - 1))				/* Did we hit the top of the scroll reg	*/
			{
				vcur_lin = vrol_top;					/* Set current line = top of scroll reg */
				vdefer(RESTORE);					/* Make sure screen is updated		*/
				vmap(SCROLL_DOWN,vrol_top,0,vrol_bot,0);		/* Scroll just the scrolling region	*/
				if (noscroll_avail)
				{
					svo = voptimize(OFF);				/* Turn optimization OFF.		*/
					for (i = vrol_bot; i >= vrol_top ; i--)
					{
						vmove(i,0);
						memcpy(temp,vchr_map[i],MAX_COLUMNS_PER_LINE);
						temp[MAX_COLUMNS_PER_LINE] = '\0';
						vprint("%s",temp);
					}
					vmove(0,0);
					voptimize(svo);					/* Set back to what it was.		*/
				}
			}
			vbuffering(AUTOMATIC);						/* Back to automatic buffering.		*/
			break;
		}

		default:								/* Oops, invalid function.		*/
		{
			vre("Invalid direction specified in vnewline() or vlinefeed()");
			return(FAILURE);						/* Let the caller know we failed.	*/
		}
	}
	return(SUCCESS);
}
