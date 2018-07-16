/*
******************************************************************************
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
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



/*						Include required header files.							*/

#include <stdio.h>									/* Include video definitions.		*/

#include <stdlib.h>

#include <video.h>
#include <vlocal.h>
#include <vdata.h>
#include <vmenu.h>

#include "idsistd.h"
#include "vwang.h"
#include "wglobals.h"

static int test_end_window(char *cptr);							/* Test if end of footer area.		*/

/*						Static and Global Data Definitions.						*/
static int netc_pfhelp = FALSE;								/* Flag to avoid reentrant help.	*/
											/* Is menu pfkey help window active?	*/

struct video_menu menu_ncpfkey;								/* The generated pfkey menu pop-up.	*/
static int setptr_foot();

int gen_ncpfkey(int type, char** wsb, int num_chars, int* st_win, int* end_win)		/* Generate the MCB from vwang screen.	*/
											/* Flag if WSFNS or WSFNM call.		*/
{
	static char tmp_scn[1924];
	char *cptr, *ede_nc;
	char temptxt[65], *chkptr, *tptr;						/* Temp text hold variable.		*/
	register int i, j;
	int cnt, cont;


	if ((ede_nc = (char *) getenv("PFKEYWINDOW"))) return(FALSE);			/* Shell var set so don't want pop-up.	*/

	VL_vmenuinit(&menu_ncpfkey, DISPLAY_ONLY_MENU, VMODE_REVERSE, 0, 0, 0);		/* Initialize the menu definition.	*/

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
		cptr++;
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
				if (vwang_valid_char_data(*cptr)) 			/* Char not in displayable range.	*/
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
		VL_vmenuitem(&menu_ncpfkey, temptxt, 0 , NULL);				/* Init the menu text.			*/
	}
	if (type) *end_win = 24 - ((num_chars - (cnt + 4))/80);				/* Set end line of pfkey window area.	*/ 
	*wsb = tmp_scn;									/* Set so will display temp screen.	*/
	return(TRUE);									/* Set to indicate pfkey window.	*/
}

/* Pop up the PFkey menu window and get	*/
/*  the pfkey pressed.			*/
int nc_pop_menu(int *filling, const char *terminate_list, unsigned char *no_mod, char *pfkey )
{
	enum e_vop op_save;								/* Optimization save word.		*/
	int choice;									/* Menu choice.				*/
	int row_current, col_current;							/* Current screen locations.		*/
	int mdsave, pfsave;								/* Menu state save words.		*/
	char *ede_nc;

	if (netc_pfhelp || (ede_nc = (char *) getenv("PFKEYWINDOW")))			/* Shell var set or already in pop-up.	*/
	{
		vwang_bad_char();								/* Ring the bells.			*/
		return(FAILURE);							/* Return to the caller.		*/
	}
	else netc_pfhelp = TRUE;							/* Now Netroncap pfkey help is active.	*/
	VL_vdefer_restore();								/* Cannot be in deferred mode.		*/

	row_current = vcur_lin+1;							/* Remember where on Wang screen.	*/
	col_current = vcur_col+1;

	op_save = voptimize(VOP_DEFER_MODE);						/* Make sure optimization is on.	*/
	VL_vmenustatus(&mdsave, &pfsave);							/* Get the menu status.			*/
	VL_vmenu_pfkeys(ON);								/* Turn PF key processing on.		*/
	VL_vmenumode(STATIC_MENU);								/* This is a static menu.		*/
	choice = VL_vmenugo(&menu_ncpfkey);						/* Display the menu.			*/
	VL_vmenu_pfkeys(pfsave);								/* Restore the PF keys.			*/
	VL_vmenumode(mdsave);								/* Restore the menu mode.		*/

	if (VL_vfnkey(choice))								/* Is the key pressed a function key?	*/
	{
		vwang_ws_fkey(choice,filling,terminate_list,pfkey,no_mod);
	}
	voptimize(op_save);								/* Put the optimization back as it was.	*/
	netc_pfhelp = FALSE;								/* Now Netroncap pfkey help is inactive.*/
	VL_synch_required = FALSE;								/* A synch is not required.		*/

	if (*filling) return(FAILURE);							/* Not a valid pfkey.			*/
	else 	return(SUCCESS); 							/* All done.				*/
}

static int setptr_foot(cnt,cptr,max)							/* Call from WSFMS so set ptr to footer.*/
int *cnt, max;
char **cptr;
{
	char cmp_line[80], the_char;
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
static int test_end_window(char *cptr)								/* Test if end of footer area.		*/
{
	char cmp_line[81];

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
/*
**	History:
**	$Log: edenetc.c,v $
**	Revision 1.22  2003/06/27 15:54:03  gsl
**	fix EDE API
**	
**	Revision 1.21  2003/06/23 17:28:54  gsl
**	static func
**	
**	Revision 1.20  2003/06/23 15:28:04  gsl
**	VL_ global symbols
**	
**	Revision 1.19  2003/02/05 21:47:53  gsl
**	fix -Wall warnings
**	
**	Revision 1.18  2003/02/04 18:57:00  gsl
**	fix copyright header
**	
**	Revision 1.17  2002/08/01 15:07:37  gsl
**	type warnings
**	
**	Revision 1.16  2002/07/15 20:16:05  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.15  2002/07/15 14:07:03  gsl
**	vwang globals
**	
**	Revision 1.14  2002/07/12 20:40:38  gsl
**	Global unique WL_ changes
**	
**	Revision 1.13  2002/07/11 20:29:20  gsl
**	Fix WL_ globals
**	
**	Revision 1.12  2002/07/09 04:14:07  gsl
**	Rename global WISPLIB routines WL_ for uniqueness
**	
**	Revision 1.11  1997/07/08 20:12:09  gsl
**	Change to use new video.h defines
**	
**	Revision 1.10  1996-09-13 13:54:03-04  gsl
**	fix warnings for NT
**
**
**
*/
