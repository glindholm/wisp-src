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

/*						Static data definitions.							*/

static int save_lin, save_col, save_atr, save_chs;					/* Save locations.			*/
static int state();

/*						Subroutine entry point.								*/

int vrefresh(what) int what;								/* Refresh screen from maps.		*/
{
	register int ret;								/* Return code.				*/

	switch (what)									/* What are we to do.			*/
	{
		case FULL_SCREEN:  {ret = vre_full(); break;}				/* Do a regular refresh.		*/
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

int vre_hard()
{
	int old_lin_cng[MAX_LINES_PER_SCREEN];						/* Old change control (before clear).	*/
	int old_atr[MAX_LINES_PER_SCREEN];						/* Old line attributes (before clear).	*/
	int old_op;									/* Save locations.			*/
	register int i, j, k;								/* Working registers.			*/
	register char c;								/* Working characters.			*/

	if (deferred)									/* Are we in deferred mode?		*/
	{
		vre("vrefresh(HARD_REFRESH)-Internal error, cannot hard refresh while in deferred mode.");
		return(FAILURE);							/* Don't let things go past here.	*/
	}
	
	state(SAVE);									/* Save everything.			*/
	old_op = optimization;								/* Remember how we were optimizing.	*/
	optimization = OFF;								/* Now we don't want any optimization.	*/
	vbuffering(LOGICAL);								/* Turn on logical buffering.		*/

	imemcpy(old_lin_cng, vlin_cng, MAX_LINES_PER_SCREEN);				/* Remember the line change state.	*/
	imemcpy(old_atr, vlin_atr, MAX_LINES_PER_SCREEN);				/* Remember the line attributes.	*/

	vmove(0,0);									/* Avoid cursor dashing.		*/
	for (i = 0; i < INT_SET_TABLE_SIZE; i++) vset(i,vcur_set[i]);			/* Restore the current settings.	*/
	vscreen(vscr_atr);								/* Reset screen, note may clear screen.	*/

	for (i = 0; i < MAX_LINES_PER_SCREEN; i++)					/* Reset line sizes as appropriate.	*/
	{
		if (vlin_atr[i] != old_atr[i])						/* Is the size other than single?	*/
		{
			vmove(i,0);							/* Yes, move to avoid cursor jumping.	*/
			vsize(i,old_atr[i]);						/* Set the line to that size.		*/
		}
	}

	optimization = DEFER_MODE;							/* Now we can defer things.		*/
	vmov_op = OFF;									/* Force no optimization 1st time.	*/
	vchs_op = OFF;
	vmod_op = OFF;

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

	vdefer(RESTORE);								/* Cannot leave in deferred mode.	*/
	optimization = TRACKING_ONLY;							/* Turn optimization off again.		*/
	vroll(vrol_top,vrol_bot);							/* Reset the scrolling region.		*/
	state(RESTORE);									/* Restore where we were.		*/
	optimization = old_op;								/* Put optimization back on too.	*/
	vbuffering(AUTOMATIC);								/* Restore automatic buffering.		*/
	return(SUCCESS);								/* And we're done.			*/
}

/*						Do full screen optimized refresh.						*/

int vre_full()										/* Full screen optimized refresh.	*/
{
	int save_atr, save_chr_set;							/* Save character set data.		*/
	int first, fcol, ecol;								/* Local variables.			*/
	int x0, y0, m0, c0;								/* Current position.			*/
	int ebos_needed;								/* Flag to indicate erase from bos rqd.	*/
	register int i, j, k, f, t;							/* Working registers.			*/
	char c[2];									/* Working character.			*/

	c[0] = 0;									/* Initialize working chars to 0.	*/
	c[1] = 0;

	save_atr = vcur_atr;								/* Save the rendition and char set.	*/
	save_chr_set = vchr_set;

	x0 = tcur_lin;									/* Record where we are.			*/
	y0 = tcur_col;
	m0 = tcur_atr;
	c0 = tchr_set;

	ebos_needed = FALSE;								/* Was erase from begin ever needed.	*/
	first = TRUE;									/* First visible data not located.	*/
	for (i = 0; i < MAX_LINES_PER_SCREEN; i++)					/* Loop through every line.		*/
	{
		k = vml(i);								/* Compute the index into map.		*/
		if (f = from_bol_needed(i, k, &fcol))					/* Any old (deleted) data on this line?	*/
		{
			ebos_needed = TRUE;						/* Data now found that must be erased.	*/
			if (fcol < vedge(i))						/* Yes, then visible data on the line?	*/
			{
				vre_move(i, fcol, k, &x0, &y0, m0);			/* Yes, then move its location.		*/
				if (first)						/* First visible data found on screen?	*/
				{
					vre_erase(FROM_BOS);				/* Yes, erase from start of screen.	*/
					first = FALSE;					/* No longer the first time.		*/
				}
				else vre_erase(FROM_BOL);				/* No, erase from start of line.	*/
			}
			else if (!first)						/* No visible data. First time?		*/
			{
				vre_move(i, 0, k, &x0, &y0, m0);			/* Yes, move to where the data is.	*/
				vre_erase(TO_EOL);					/* Erase to the end of the line.	*/
			}
		}

		if (fcol < vedge(i))							/* Anything to do on this line?		*/
		{
			t = to_eol_needed(i, k, &ecol);					/* Determine if erase to eol needed.	*/
			for (j = fcol; j <= ecol; j++)					/* Now output the actual text.		*/
			{
				if (vmap_cng[k][j] != 0)				/* Does this data have to change?	*/
				{
					vre_move(i, j, k, &x0, &y0, m0);		/* Yes so move to it.			*/
					if (first && ebos_needed)			/* First visible data found on screen?	*/
					{
						vre_erase(FROM_BOS);			/* Yes, erase from start of screen.	*/
						first = FALSE;				/* No longer the first time.		*/
					}
					c[0] = vchr_map[k][j];				/* Get the character in the map.	*/
					if (vmap_cng[k][j] < 0) 			/* Is this old data?			*/
					{
						c[0] = ' ';				/* Yes character should be space.	*/
						vatr_map[k][j] = 0;			/* And attributes are no longer valid.	*/
					}
					vmap_cng[k][j] = 0;				/* Now considr map up to date.		*/
					vchr_map[k][j] = c[0];				/* Record the character there.		*/

					if (c0 != vmaskc(vatr_map[k][j]))		/* Is the current char set selected?	*/
					{
						vcharset(vmaskc(vatr_map[k][j])); 	/* Select the character set.		*/
						c0 = vmaskc(vatr_map[k][j]);		/* Record what we are switching to.	*/
					}

					if (m0 != vmaskm(vatr_map[k][j]))		/* Is the current mode selected?	*/
					{
						vmode(vmaskm(vatr_map[k][j]));  	/* Select the mode.			*/
						m0 = vmaskm(vatr_map[k][j]);		/* Remember what were at.		*/
					}

					vcontrol(c);					/* Output the character.		*/
					if (y0+1 < vedge(i)) y0 = y0 + 1;		/* Update the cursor positioning stuff.	*/
				}
				if (first && visible(vchr_map[k][j],vatr_map[k][j])) first = FALSE;	/* Visible data found.	*/
			}
			if (t)								/* Did we have to erase to end of line?	*/
			{
				vre_move(i, ecol+1, k, &x0, &y0, m0);			/* Move to the end of visible data.	*/
				vre_erase(TO_EOL);					/* Now, if necessary, erase to eol.	*/
			}
		}
	}

	if (first && ebos_needed) vre_erase(FULL_SCREEN);				/* No changes at all so just erase.	*/

	tcur_lin = x0;									/* Restore global position tracking.	*/
	tcur_col = y0;
	tcur_atr = m0;
	tchr_set = c0;
	vcur_atr = save_atr;
	vchr_set = save_chr_set;

	return(SUCCESS);								/* Return, return, return, return.	*/
}

int from_bol_needed(i, k, col) register int i, k, *col;					/* Determine if erase from bol needed.	*/
{
	register int j, old, found, v;							/* Working registers.			*/

	old = FALSE;									/* Assume no old data.			*/
	found = FALSE;									/* Visible data not found.		*/
	j = -1;										/* Start one before 0.			*/

	while (!found && (j++ < vedge(i)))						/* Loop until found or at right edge.	*/
	{
		v = visible(vchr_map[k][j],vatr_map[k][j]);				/* Determine if character is visible.	*/
		if ((vmap_cng[k][j] < 0) || !v)						/* Is this an old or invisible char?	*/
		{
			if (vmap_cng[k][j] < 0) old = TRUE;				/* Is this old data?			*/
			if ((vmap_cng[k][j] != 0) && !v) old = TRUE;			/* An updated space also counts as old.	*/
			vmap_cng[k][j] = 0;						/* Clear this map element.		*/
			vatr_map[k][j] = 0;
			vchr_map[k][j] = ' ';
		}
		else found = TRUE;							/* Ah ha, we found visible data.	*/
	}
	if ((*col = (j-1)) < 0) *col = 0;						/* Return where we are.			*/
	return(old);									/* Return the state of old data.	*/
}

int to_eol_needed(i, k, col) register int i, k, *col;					/* Determine if erase to eol needed.	*/
{
	register int j, old, found, v;							/* Working registers.			*/

	old = FALSE;									/* Assume old data won't be found.	*/
	found = FALSE;									/* Visible data not found yet.		*/
	j = vedge(i);									/* Start on the left side of screen.	*/

	while (!found && (--j >= 0))							/* Loop through each column.		*/
	{
		v = visible(vchr_map[k][j],vatr_map[k][j]);				/* Is the character visible?		*/
		if ((vmap_cng[k][j] < 0) || !v)						/* Is char old or invisible.		*/
		{
			if (vmap_cng[k][j] < 0) old = TRUE;				/* If taged as old, old found.		*/
			if ((vmap_cng[k][j] != 0) && !v) old = TRUE;			/* Updated spaces also count as old.	*/
			vmap_cng[k][j] = 0;						/* Clear the screen map.		*/
			vatr_map[k][j] = 0;
			vchr_map[k][j] = ' ';
		}
		else found = TRUE;							/* Found visible data.			*/
	}

	*col = j;									/* Return last column number.		*/
	return(old);									/* Return if old data found.		*/
}

vre_erase(what) int what;								/* Erase all or part of screen.		*/
{
#ifdef	MSDOS
	vrawerasetype( what );								/* Do erase in vrawdos.c		*/
#else	/* VMS or unix */
	switch(what)									/* Determine what to erase.		*/
	{
		case FULL_SCREEN: {vcontrol(efuls_esc); break;}				/* Full screen.				*/
		case FROM_BOS:	  {vcontrol(efbos_esc); break;}				/* From beginning of screen.		*/
		case TO_EOS:	  {vcontrol(eteos_esc); break;}				/* To end of screen.			*/
		case FROM_BOL:	  {vcontrol(efbol_esc); break;}				/* From beginning of line.		*/
		case TO_EOL:	  {vcontrol(eteol_esc); break;}				/* To end of line.			*/
	}
#endif	/* VMS or unix */
	return(SUCCESS);								/* Return to the caller.		*/
}

vre_move(i, j, k, x0, y0, m0) register int i, j, k, m0; int *x0, *y0;			/* Move to a location.			*/
{
	char string[12];		
#ifdef unix
	char *tparm();
#define PARMFUNC tparm
#else
	char *vcparm();
#define PARMFUNC vcparm
#endif

	if ((i == *x0) && (j == *y0)) return(OPTIMIZED);				/* Don't move if already there.		*/
	else if ((i == (*x0)+1) && (j == 0)) vcontrol("\n");				/* Use a new-line to get to next line.	*/
	else if ((i == *x0) && (j == *(y0)+1) && !(m0 & (vis_space)) &&
		!visible(vchr_map[k][j-1],vatr_map[k][j-1])) vcontrol(" ");		/* Move using a space.			*/
	else
	{
#ifdef MSDOS
		vrawmove(i,j);
#else	/* VMS or unix */
		vcontrol(PARMFUNC(vcapdef[CURSOR_ADDRESS],i,j));
#endif	/* VMS or unix */
#if 0
		if (!vmovebias) sprintf(string,mvrowcol_esc,i+1,j+1);			/* Convert to a string.			*/
		else sprintf(string,mvrowcol_esc,i+vmovebias,j+vmovebias);		/* Convert to a string.			*/
		vcontrol(string);							/* Position the cursor.			*/
#endif
	}
	*x0 = i;									/* Record the new position.		*/
	*y0 = j;
	return(SUCCESS);								/* Return to the caller.		*/
}

static int state(action) int action;							/* Perform specified action.		*/
{
	switch(action)									/* Select the action.			*/
	{
		case SAVE:								/* Save current setup and status.	*/
		{
			save_lin = vcur_lin;						/* Save position.			*/
			save_col = vcur_col;
			save_atr = vcur_atr;						/* Save character rendition.		*/
			save_chs = vchr_set;						/* Save character set.			*/
			break;
		}

		case RESTORE:								/* Restore old setup and status.	*/
		{
			vmode(save_atr);						/* Restore old character rendition.	*/
			vcharset(save_chs);						/* Restore character set.		*/
			vmove(save_lin,save_col);					/* Restore old position.		*/
			break;
		}
	}
}
