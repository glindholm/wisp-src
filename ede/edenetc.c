			/************************************************************************/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/


/*						Include required header files.							*/

#include <stdio.h>									/* Include video definitions.		*/

#ifndef unix	/* VMS or MSDOS */
#include <stdlib.h>
#endif

#include <v/video.h>
#include <v/vlocal.h>
#include <v/vdata.h>
#include <v/vmenu.h>
#include "vwang.h"
#include "wglobals.h"

/*						Static and Global Data Definitions.						*/
extern int netc_pfhelp;									/* Is menu pfkey help window active?	*/
struct video_menu menu_ncpfkey;								/* The generated pfkey menu pop-up.	*/

int gen_ncpfkey(type,wsb,num_chars,st_win,end_win)					/* Generate the MCB from vwang screen.	*/
int type, num_chars, *st_win, *end_win;							/* Flag if WSFNS or WSFNM call.		*/
char **wsb;
{
	static char tmp_scn[1924];
	char *cptr, *ede_nc;
	char temptxt[65], *chkptr, *tptr;						/* Temp text hold variable.		*/
	register int i, j;
	int cnt, len, cont;


	if (ede_nc = (char *) getenv("PFKEYWINDOW")) return(FALSE);			/* Shell var set so don't want pop-up.	*/

	vmenuinit(&menu_ncpfkey, DISPLAY_ONLY_MENU, REVERSE, 0, 0, 0);			/* Initialize the menu definition.	*/

	memset(tmp_scn,' ',1924);							/* Set temp area to blank.		*/
	cptr = *wsb;									/* Point to screen passed in.		*/
	memcpy(tmp_scn,cptr,num_chars);							/* Copy screen to temp.			*/
	num_chars++;									/* Set so doesn't go 1 too far.		*/
	cptr = tmp_scn;									/* Set local pointer to screen.		*/
	cptr += 4;									/* Set pointer past the order area.	*/
	cnt = 0;
	if (type)									/* Call from WSFMS so set ptr to footer.*/
	{
		int gotit;

		gotit = setptr_foot(&cnt,&cptr,num_chars);				/* Step to the footer area.		*/
		if (!gotit) return(FALSE);						/* Didn't get menu so return.		*/
		*st_win = 23 - ((num_chars - (cnt + 4))/80);				/* Set start line of pfkey window area.	*/
	}
	while ( ((*cptr & FAC_CHARACTER) != FAC_CHARACTER) ||				/* Set ptr to underline FAC of footer.	*/
		 ((*cptr & FAC_UNDERSCORE) != FAC_UNDERSCORE) )
	{
		*cptr++;
		cnt++;									/* Increment number of characters used.	*/
	}
	*cptr++ = ' ';									/* Step past the FAC.			*/
	while (*cptr++ == ' ') cnt++;							/* Set ptr past footer line seperator.	*/
	while ((*cptr & FAC_CHARACTER) == FAC_CHARACTER)				/* Test if any leading FAC characters.	*/
	{
		*cptr++ = ' ';								/* Set pointer to beginning of text.	*/
		cnt++;									/* Increment number of characters used.	*/
	}
	cont = TRUE;									/* Ptr is now set to beginning of text.	*/
	while ( (cnt < num_chars) && cont )						/* Copy text until end of footer area.	*/
	{
		memset(temptxt,' ',MAX_MENU_WIDTH);					/* Init the string to spaces.		*/
		tptr = temptxt;								/* Point to beginning of text field.	*/
		i = 0;									/* Set to 0 chars in temp string.	*/
		while ( i < MAX_MENU_WIDTH && cont)					/* Copy text until max avail width.	*/
		{
			if (!(cont = test_end_window(cptr))) break;			/* Test if end of footer area.		*/
			*tptr++ = *cptr;						/* Copy first char to buffer.		*/
			*cptr++ = ' ';							/* Blank out the footer area.		*/
			i++;
			cnt++;
			while ((*cptr != '(') && (cnt < num_chars) && (i < MAX_MENU_WIDTH)) /* Copy text to next PFkey def.	*/
			{
				if (!(cont = test_end_window(cptr))) break;		/* Test if end of footer area.		*/
				if (*cptr < SPACE_BAR || *cptr >= MAX_DISP_RANGE) 	/* Char not in displayable range.	*/
				{
					*cptr = ' ';					/* So, set to a space.			*/
				}
				*tptr++ = *cptr;					/* Copy char to buffer.			*/
				*cptr++ = ' ';						/* Blank out the footer area.		*/
				cnt++;							/* Increment number of characters used.	*/
				i++;							/* Increment num chars used in temp buf.*/
			}
			if ( cnt >= num_chars || !cont)
			{
				break;							/* Have reached end of footer area.	*/
			}
			else								/* Check if will fit in current buffer.	*/
			{
				chkptr = cptr;						/* Set check ptr to current position.	*/
				chkptr++;						/* Step past open paren.		*/
				j = 1;                           
				while (*chkptr !=  '(' && ((i+j) < num_chars))		/* Test to see if have room in buffer.	*/
				{
					chkptr++;
					j++;
					if ((i + j) > MAX_MENU_WIDTH) break;		/* Don't have room so break test.	*/
					if (!(cont = test_end_window(cptr))) break;	/* Test if end of footer area.		*/
				}
				if ((i + j) >= MAX_MENU_WIDTH) i += j;			/* Don't have room so end buffer.	*/
			}
		}
		*tptr = '\0';								/* Null terminate the string.		*/
		vmenuitem(&menu_ncpfkey, temptxt, 0 , NULL);				/* Init the menu text.			*/
	}
	if (type) *end_win = 24 - ((num_chars - (cnt + 4))/80);				/* Set end line of pfkey window area.	*/ 
	*wsb = tmp_scn;									/* Set so will display temp screen.	*/
	return(TRUE);									/* Set to indicate pfkey window.	*/
}

int nc_pop_menu(filling,terminate_list,no_mod,pfkey)					/* Pop up the PFkey menu window and get	*/
unsigned char *terminate_list, *no_mod, *pfkey;						/*  the pfkey pressed.			*/
int *filling;
{
	int op_save;									/* Optimization save word.		*/
	int choice;									/* Menu choice.				*/
	int row_current, col_current;							/* Current screen locations.		*/
	unsigned char *ssave, *vsss();							/* Screen save.				*/
	int mdsave, pfsave;								/* Menu state save words.		*/
	char *ede_nc;

	if (netc_pfhelp || (ede_nc = (char *) getenv("PFKEYWINDOW")))			/* Shell var set or already in pop-up.	*/
	{
		ws_bad_char();								/* Ring the bells.			*/
		return(FAILURE);							/* Return to the caller.		*/
	}
	else netc_pfhelp = TRUE;							/* Now Netroncap pfkey help is active.	*/
	vdefer(RESTORE);								/* Cannot be in deferred mode.		*/

	row_current = vcur_lin+1;							/* Remember where on Wang screen.	*/
	col_current = vcur_col+1;

	op_save = voptimize(DEFER_MODE);						/* Make sure optimization is on.	*/
	vmenustatus(&mdsave, &pfsave);							/* Get the menu status.			*/
	vmenu_pfkeys(ON);								/* Turn PF key processing on.		*/
	vmenumode(STATIC_MENU);								/* This is a static menu.		*/
	choice = vmenugo(&menu_ncpfkey);						/* Display the menu.			*/
	vmenu_pfkeys(pfsave);								/* Restore the PF keys.			*/
	vmenumode(mdsave);								/* Restore the menu mode.		*/

	if (vfnkey(choice))								/* Is the key pressed a function key?	*/
	{
		ws_fkey(choice,filling,terminate_list,pfkey,no_mod);
	}
	voptimize(op_save);								/* Put the optimization back as it was.	*/
	netc_pfhelp = FALSE;								/* Now Netroncap pfkey help is inactive.*/
	synch_required = FALSE;								/* A synch is not required.		*/

	if (*filling) return(FAILURE);							/* Not a valid pfkey.			*/
	else 	return(SUCCESS); 							/* All done.				*/
}

static int setptr_foot(cnt,cptr,max)							/* Call from WSFMS so set ptr to footer.*/
int *cnt, max;
char **cptr;
{
	char cmp_line[80], *lptr, the_char;
	int line;
	memset(cmp_line,' ',80);
	for (line=14; line<24; ++line)
	{
		the_char = (*cptr)[line*80];
		if (((the_char & FAC_CHARACTER) == FAC_CHARACTER) &&	
		  ((the_char & FAC_UNDERSCORE) == FAC_UNDERSCORE) &&
		    !memcmp((*cptr)+line*80+1,cmp_line,78) )
		{
			*cptr += line*80;
			*cnt += line*80;
			return TRUE;
		}
		the_char = (*cptr)[line*80+1];
		if (((the_char & FAC_CHARACTER) == FAC_CHARACTER) &&	
		  ((the_char & FAC_UNDERSCORE) == FAC_UNDERSCORE) &&
		    !memcmp((*cptr)+line*80+2,cmp_line,78) )
		{
			*cptr += line*80+1;
			*cnt += line*80+1;
			return TRUE;
		}
	}
	return FALSE;
}
int test_end_window(cptr)								/* Test if end of footer area.		*/
char *cptr;
{
	char cmp_line[81], *lptr;

	memset(cmp_line,' ',80);							/* Set compare line to blanks.		*/
	cmp_line[80] = '\0';								/* Null terminate the string.		*/

	if ( ((*cptr & FAC_CHARACTER) == FAC_CHARACTER) &&
	     ((*cptr & FAC_UNDERSCORE) == FAC_UNDERSCORE) )				/* Test if UNDERLINE FAC.		*/
	{
		cptr++;									/* Step past FAC.			*/
		if (!memcmp(cptr,cmp_line,78)) return(FALSE);				/* Is footer seperator so set to end.	*/
	}
	return(TRUE);									/* Is not end so set to continue.	*/
}
