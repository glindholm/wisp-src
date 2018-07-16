			/************************************************************************/
			/*	      VIDEO - Video Interactive Development Environment		*/
			/*			Copyright (c) 1988, 1989, 1990			*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/

/*					Include required header files.								*/

#include "video.h"									/* Get the standard VIDEO definitions.	*/
#include "vlocal.h"									/* Get the local video definitions.	*/


/*					Allowable embedded control characters.							*/

#define	HT	'\t'									/* Horizontal tab.			*/
#define	NL	'\n'									/* New line (line feed).		*/
#define	CR	'\r'									/* Carriage return.			*/
#define	BS	'\b'									/* Back space.				*/
#define BL	'\007'									/* Bell character.			*/


/*					Static data.										*/

int dangled = TRUE;									/* Flag to indicate output left over.	*/
char last_chr = 0;									/* Remember the last character.		*/

static int tab_stop();

/*					Subroutine entry point.									*/

int vprint(format,a0,a1,a2,a3,a4,a5,a6,a7)
	char *format;									/* Pointer to format control string.	*/
	long a0,a1,a2,a3,a4,a5,a6,a7;							/* Up to 8 arguments allowed.		*/
{
	extern int vcur_lin, vcur_col, tcur_lin, tcur_col;				/* Reference true screen position.	*/
	extern int synch_required, new_screen, optimization, deferred;			/* Optimization control flags.		*/
	extern int vpri_op, vmov_op;							/* Print and move optimization flags.	*/
	int lin_save, col_save;								/* To save position values.		*/
	register int i, j, rstat;							/* Working registers.			*/
	unsigned char ascii[PRINT_BUFFER_SIZE];						/* Expanded "pure" ascii string.	*/
	vbuffering(LOGICAL);								/* Turn on logical buffering.		*/
	sprintf(ascii,format,a0,a1,a2,a3,a4,a5,a6,a7);					/* Convert to a pure string.		*/

	if (!optimization) rstat = vrawprint(ascii);					/* Are we to track what is going on?	*/

	else
	{
		synch_required = TRUE;							/* Now output has been done.		*/
		new_screen = FALSE;							/* Next screen cannot now be new.	*/
		rstat = SUCCESS;							/* Assume we will optimize.		*/
		lin_save = vcur_lin;							/* Remember where this I/O starts.	*/
		col_save = vcur_col;
		last_chr = 0;								/* No last character yet.		*/

		for (i = 0; ((i < PRINT_BUFFER_SIZE) && (ascii[i] != 0) && (rstat == SUCCESS)); i++)
		{
			dangled = TRUE;							/* Assume output will be dangled.	*/

			if (ascii[i] >= ' ') rstat = vpr_put_map(ascii[i]);		/* Put non-control chars into map.	*/
			else
			{
				last_chr = 0;						/* Reset the last character.		*/
				switch (ascii[i])					/* Select the control function.		*/
				{
					case NL: {rstat = vpr_nl_map(); break;}		/* New line?				*/
					case HT: {rstat = vpr_ht_map(); break;}		/* Horizontal tab?			*/
					case CR: {rstat = vpr_cr_map(); break;}		/* Carriage return?			*/
					case BS: {rstat = vpr_bs_map(); break;}		/* Back space?				*/
					case BL: {rstat = SUCCESS; break;}		/* Let the bells toll.			*/
					default:					/* Oops, invalid character.		*/
					{
#ifdef	MSDOS
											/* All characters display on MSDOS.	*/
											/* All control characters other than	*/
											/* NL, HT, CR, BS, and BL will be sent	*/
											/* to the screen for display.		*/
											/*					*/
											/* Character		MSDOS Graphic	*/
											/* ---------		-------------	*/
											/* NL: New Line		= Solid Box	*/
											/* HT: Tab		= Open Circle	*/
											/* CR: Carriage Return	= Musical Note	*/
											/* BS: Back Space	= Open Box	*/
											/* BL: Bell		= Large Dot	*/
											/*					*/
											/* Ctrl-D		= Solid Diamond	*/
											/* (This one is used as a pseudo blank)	*/
											/*					*/
											/* See MSDOS documentation for graphic	*/
											/* descriptions of other control chars.	*/
											/*					*/
						rstat = vpr_put_map(ascii[i]);		/* Put all characters into map.		*/
#else	/* VMS or unix */
						unsigned int tmp;

						tmp = ascii[i];				/* Convert to an integer.		*/
						vre("vprint()-Invalid control character (0x%2X) in output text.",tmp);
						rstat = FAILURE;
#endif	/* VMS or unix */
						break;					/* Exit the conditional.		*/
					}
				}
			}
		}

		if (!vpri_op || optimization <= TRACKING_ONLY)				/* Should print be optimized?		*/
		{
			i = vcur_lin;							/* Save the current position.		*/
			j = vcur_col;
			vcur_lin = lin_save;						/* Restore to start of the I/O.		*/
			vcur_col = col_save;
			vdefer(RESTORE);						/* Must restore just in case.		*/
			rstat = vrawprint(ascii);					/* No, so just print the text.		*/
			vcur_lin = i;							/* Restore where the I/O took us.	*/
			vcur_col = j;
			if (!vpri_op)							/* Did true position change?		*/
			{
				tcur_lin = vcur_lin;					/* Yes, then record the event.		*/
				tcur_col = vcur_col;
			}
			if (vmov_op) vpri_op = ON;					/* Turn on optimization if move op on.	*/
		}

		else if (dangled)							/* Was there output left over?		*/
		{
			if (optimization >= DATA_CONTROLS_AND_MOTION)			/* Should we defer?			*/
			{
				if (!deferred) deferred = MOTION_ONLY;			/* Enter deferred mode.			*/
			}
			else vpr_goto(vcur_lin,vcur_col,tcur_lin,tcur_col);		/* Just data so move before exit.	*/
		}
	}

	vbuffering(AUTOMATIC);								/* Restore to automatic buffering.	*/
	return(rstat);
}
/*					Put a character in the character map.							*/

int vpr_put_map(c) char c;
{
	extern int vcur_col, vcur_lin, vcur_atr, vchr_set;				/* Reference VIDEO data base.		*/
	extern int tcur_lin, tcur_col;							/* Reference deferred locations.	*/
	extern char vchr_map[MAX_LINES_PER_SCREEN][MAX_COLUMNS_PER_LINE];		/* Reference character map.		*/
	extern char vatr_map[MAX_LINES_PER_SCREEN][MAX_COLUMNS_PER_LINE];		/* Reference attribute map.		*/
	extern char vmap_cng[MAX_LINES_PER_SCREEN][MAX_COLUMNS_PER_LINE];		/* Reference change map.		*/
	extern int vpri_op, optimization, deferred;					/* Optimization control.		*/
	register char *chr, *atr, *cng;							/* Working pointers.			*/
	register int i,rstat;								/* Working integers.			*/

	i = vml(vcur_lin);								/* Get array position.			*/
	chr = &vchr_map[i][vcur_col];							/* Point to character map element.	*/
	atr = &vatr_map[i][vcur_col];							/* Point to attribute map element.	*/
	cng = &vmap_cng[i][vcur_col];							/* Point to change map element.		*/

	if ((*chr == c) && (*atr == (vcur_atr | vchr_set)))				/* Is the map changing?			*/
	{
		if (!((deferred == BLOCK_MODE) && (*cng == 1))) *cng = 0;		/* No change.				*/
		rstat = vpr_inc_col();							/* Increment the column number.		*/
		if (visible(*chr,*atr)) vic(vcur_lin);					/* Increment global change counts.	*/
	}

	else if (!visible(c,vcur_atr) && !visible(*chr,*atr))				/* Visible now and before?		*/
	{
		*chr = c;								/* Store the current char.		*/
		*atr = vcur_atr | vchr_set;						/* Store the current attributes.	*/
		rstat = vpr_inc_col();							/* Increment the column number.		*/
	}

	else										/* Map changed so...			*/
	{
		*chr = c;								/* Store the current char.		*/
		*atr = vcur_atr | vchr_set;						/* Store the current attributes.	*/
		if (visible(*chr,*atr)) vic(vcur_lin);					/* Increment global change counts.	*/

		if (vpri_op && (optimization >= DATA_ONLY) && !((deferred == BLOCK_MODE) && (*cng != 1)))
		{
			if (deferred) vdefer(RESTORE);					/* Restore the state.			*/
			else vpr_goto(vcur_lin,vcur_col,tcur_lin,tcur_col);		/* Just move to the location.		*/

			vrawputc(c);							/* Output the character.		*/
			rstat = vpr_inc_col();						/* Increment the column number.		*/
			if (rstat) vpr_true();						/* Track the true column too.		*/
			dangled = FALSE;						/* And now, no output left over.	*/
		}
		else rstat = vpr_inc_col();						/* Increment the column number.		*/
		if (deferred == BLOCK_MODE) *cng = 1;					/* Store the fact we've changed.	*/
	}

	last_chr = c;									/* Remember the last character.		*/
	return(rstat);									/* Return the results.			*/
}
/*					Record the occurance of a back space character.						*/

int vpr_bs_map()
{
	extern int vcur_col;								/* References to VIDEO data base.	*/

	vcur_col = vcur_col - 1;							/* Back up one space.			*/

	if (vcur_col < 0)								/* Over left hand side of screen?	*/
	{
		vcur_col = 0;								/* Yes, then reset to zero.		*/
		vre("vprint()-Backspace characters in output text went off of the left hand side of the screen.");
		return(FAILURE);							/* And return with failure.		*/
	}

	return(SUCCESS);								/* No, then all ok so return success.	*/
}

/*				Record the occurance of a new-line character.							*/

int vpr_nl_map()
{
	extern int vcur_col, vcur_lin, vrol_bot, vrol_top;				/* Reference VIDEO data base.		*/
	extern int vb_pure;								/* Reference sense of line feed.	*/

	vcur_lin = vcur_lin + 1;							/* Set to start of next line.		*/

	if (vcur_lin-1 == vrol_bot) vpr_scroll(vrol_top,0,vrol_bot,0);			/* Scroll the scroll region.		*/
	else if (vcur_lin == MAX_LINES_PER_SCREEN) vpr_scroll(0,0,MAX_LINES_PER_SCREEN-1,0);	/* Scroll the full screen.	*/
	else if (!vb_pure) vcur_col = 0;						/* Is \n a line-feed or a new-line?	*/

	return(SUCCESS);								/* What could possibly go wrong?	*/
}

/*					Record the occurance of a horizontal tab.						*/

int vpr_ht_map()
{
	extern int vcur_col;								/* External reference current column.	*/
	register int i,j;								/* Working registers.			*/

	vpr_put_map(' ');								/* Put out at least one space.		*/
	while (!tab_stop(vcur_col)) vpr_put_map(' ');					/* Output spaces until the tab stop.	*/
	return(SUCCESS);								/* Assume all went ok so return ok.	*/
}

static int tab_stop(c) int c;
{
/*	NOTE: This assumes 8 column tabs are set on the terminal. Need a better algorithm (later).				*/
	extern int vscr_wid;								/* Reference screen width.		*/

	if ((c ==  8) || (c == 16) || (c == 24) || (c == 32)) return(TRUE);		/* Fast routine for checking for	*/
	if ((c == 40) || (c == 48) || (c == 56) || (c == 64)) return(TRUE);		/*  modulo 8.				*/
	if ((c == 72) || (c == 80) || (c == 88) || (c == 96)) return(TRUE);
	if ((c == 104) || (c == 112) || (c == 120) || (c == 128)) return(TRUE);
	if (c >= (vscr_wid-1)) return(TRUE);						/* Assume a tab at edge of screen.	*/
	return(FALSE);	
}


/*					Record the occurance of a carriage return.						*/

int vpr_cr_map()
{
	extern int vcur_col;								/* Reference VIDEO data base.		*/
	vcur_col = 0;									/* Set to the start of the row.		*/
	return(SUCCESS);								/* Nothing could go wrong, go wrong...	*/
}


/*					Subroutine to scroll the map.								*/

vpr_scroll(sl,sc,el,ec) int sl,sc,el,ec;						/* Scroll the map.			*/
{
	extern int vcur_lin, vcur_col, tcur_lin, tcur_col;				/* Current position data.		*/
	extern int vscr_cng;								/* Reference screen change count.	*/
	extern int vb_pure;								/* Reference sense of line feed.	*/
	extern int optimization, vpri_op, deferred;					/* Reference optimization flags.	*/

	vcur_lin = vcur_lin - 1;							/* Move current line back into range.	*/

	if (vpri_op && (optimization >= DATA_ONLY))					/* Should we optimize?			*/
	{
		if (deferred) vdefer(RESTORE);						/* Restore state if deferred.		*/
		else vpr_goto(vcur_lin,vcur_col,tcur_lin,tcur_col);			/* Else just go to the location.	*/
		vrawputc('\n');								/* Output new-line or line-feed.	*/
		vmap(SCROLL_UP,sl,sc,el,ec);						/* Now scroll the map up		*/
		dangled = FALSE;							/* Nothing is left over now.		*/
		if (!vb_pure) vcur_col = 0;						/* Is \n a line-feed or a new-line?	*/
		vpr_true();
	}
	else										/* Don't optimize data so...		*/
	{
		vmap(SCROLL_UP,sl,sc,el,ec);						/* Now scroll the map up		*/
		if (!vb_pure) vcur_col = 0;						/* Is \n a line-feed or a new-line?	*/
	}

	return(TRUE);									/* Output is now required.		*/
}
/*					Subroutine to increment the current column.						*/

vpr_inc_col()
{
	extern int vcur_lin, vcur_col;							/* Reference global data.		*/
	register int rstat;								/* Working registers.			*/

	vcur_col = vcur_col + 1;							/* Update the character position.	*/
	if (vcur_col >= vedge(vcur_lin)) vcur_col = vcur_col - 1;			/* Is the new position valid?		*/
	return(SUCCESS);
}


/*					Coordinate true location with current location.						*/

int vpr_true()
{
	extern int vcur_lin, vcur_col, tcur_lin, tcur_col;				/* Location parameters.			*/

	{
		tcur_lin = vcur_lin;							/* No, then remember that the current	*/
		tcur_col = vcur_col;							/*   position is the true position.	*/
	}

	return(SUCCESS);
}


/*					Local (fast) goto routine.								*/

int vpr_goto(cl,cc,tl,tc) int cl, cc, tl, tc;						/* Go to (sl,sc) from (el,ec).		*/
{
	extern char vchr_map[MAX_LINES_PER_SCREEN][MAX_COLUMNS_PER_LINE];		/* Reference character map.		*/
	extern char vatr_map[MAX_LINES_PER_SCREEN][MAX_COLUMNS_PER_LINE];		/* Reference attribute map.		*/
	extern char vmap_cng[MAX_LINES_PER_SCREEN][MAX_COLUMNS_PER_LINE];		/* Reference change map.		*/
	extern int vcur_atr, vchr_set;							/* Current character rendition.		*/
	register int temp;								/* Working registers.			*/

	if (cl == tl)									/* On the same line?			*/
	{
		if (cc == tc) return(SUCCESS);						/* Return immediate if already there.	*/
		if ((cc==tc+1)&&(last_chr==vchr_map[tl][tc])&&(vatr_map[tl][tc]==(vchr_set|vcur_atr))&&(vmap_cng[tl][tc]>=0))
		{
			vrawputc(last_chr);						/* Output the last character.		*/
			return(SUCCESS);						/* We're all done.			*/
		}
	}

	if ((cl == tl+1) && (cc == 0)) 							/* A new line?				*/
	{
		vrawputc('\n');								/* Yes, so put one out.			*/
		return(SUCCESS);							/* And we're all done.			*/
	}

#ifdef  MSDOS
	vrawmove(cl,cc);								/* Do it in vrawdos.c.			*/
#else	/* VMS or unix */
	vgoto(cl,cc,tl,tc,vmv_dist(cl,cc,tl,tc,ON),ON);					/* Have to do it the hard way.		*/
#endif	/* VMS or unix */
}
