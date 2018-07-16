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

			/************************************************************************/
			/*	      VIDEO - Video Interactive Development Environment		*/
			/*			Copyright (c) 1988, 1989, 1990			*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/


/*						Include standard header files.							*/

#include <stdio.h>									/* Include standard I/O stuff.		*/
#include <ctype.h>									/* Include character typeing routines.	*/
#include <string.h>

#include "video.h"									/* Include video definitions.		*/
#include "vlocal.h"									/* Include local definitions.		*/
#include "vdata.h"									/* Include video database.		*/
#include "vcap.h"									/* Include video key definitions.	*/
#include "vintdef.h"
#include "vmenu.h"									/* Include menu definitions.		*/
#include "vmodules.h"

/*						Static and global data.								*/

static int metapush = 0;								/* Metacharacter pushback.		*/
static int menumode = STATIC_MENU;							/* Default is static menus.		*/
static int base_lin = -1;								/* Base cursor position.		*/
static int base_col = -1;
static int pfkeys = FALSE;								/* Are PF keys active?			*/
static FILE *restore = NULL;								/* No restore in progress.		*/

static int4 vmcont(struct video_menu *mdata);						/* Continuation search routine.		*/
static int disconnect(struct video_menu *md,int item);					/* Disconnect paths and links.		*/
static int4 vmenuvector(int state, struct video_menu *mdata);				/* Vector the correct menu type.	*/
static int4 vbar(int state, struct video_menu *md);					/* Do a bar menu.			*/
static int4 vmx(int state, struct video_menu *md);					/* Pull down menu processing.		*/
static int padout(char *text, int width, int do_line);					/* Output padded text.			*/
static int vlff(int state, char *file);							/* Load menu data from file.		*/
static int tx(char *out, char *in);	 						/* Setup a string for C usage.		*/
static int lx(char *string);	 							/* Get the length of a string.		*/
static int adj(int *bmode, int *imode);							/* Change the mode to match background.	*/
static int vdo(int state, struct video_menu *md);					/* Pull down menu processing.		*/
static int detpos(int op, int4 *row, int4 *col, int width, int items);			/* Determine the position for a window.	*/
static int givehelp(void);
static int vmrest(void);								/* Is a restore in progress?		*/
static int padout2(char *text, int width, int linking, int options, struct video_menu *link);
static int padout3(char *text, int width, int graphics_1st);				/* Output padded text.			*/


/*						Subroutine entry point.								*/

int4 VL_vmenugo(struct video_menu *mdata)							/* Display a menu.			*/
{
	int4 choice;									/* Menu choice returned.		*/
	int4 save_t, save_c, save_w;							/* Menu type save location.		*/

	choice = 0;									/* Assume no choice.			*/
	save_c = mdata->column;
	save_w = mdata->width;
	if ( !(save_t = mdata->type)) 							/* Assume a bar menu if not given.	*/
	{
		mdata->type = BAR_MENU;							/* Set the default type.		*/
		if (mdata->column == 0)							/* Set the default values.		*/
		{
			mdata->column = 2;
			mdata->width = 3;
		}
	}

	choice = vmenuvector(0, mdata);							/* Select the correct menu.		*/

	mdata->type = save_t;								/* Restore the menu type as it was.	*/
	mdata->column = save_c;
	mdata->width = save_w;
	return(choice);									/* Return the pointer.			*/
}

int4 VL_vmenucont(struct video_menu *mdata)						/* Continue displaying menu's.		*/
{
	int4 choice;									/* Menu choice returned.		*/
	int4 save_t, save_c, save_w;							/* Menu type save location.		*/
	int svcurlin, svcurcol, svcurmod, svchrset;					/* Status save words.			*/

	choice = 0;									/* No choice yet.			*/
	save_c = mdata->column;
	save_w = mdata->width;
	if ( !(save_t = mdata->type)) 							/* Assume a bar menu if not given.	*/
	{
		mdata->type = BAR_MENU;							/* Set the default type.		*/
		if (mdata->column == 0)							/* Set the default values.		*/
		{
			mdata->column = 2;
			mdata->width = 3;
		}
	}

	svcurlin = vcur_lin;								/* Save basic status.			*/
	svcurcol = vcur_col;
	svcurmod = vcur_atr;
	svchrset = vchr_set;

up:	if (mdata->path != NULL) choice = vmcont(mdata->path);				/* Recurse until back at the top.	*/
	else if (mdata->save != NULL) choice = vmenuvector(1, mdata);			/* Should we really continue?		*/
	else										/* This is an error.			*/
	{
		vre("VL_vmenucont-Menu not active, cannot continue. Use VL_vmenugo.");	/* Report the error.			*/
		return(FAILURE);							/* Return with failure.			*/
	}

	if ((choice == -1) && (mdata->type == BAR_MENU)) verase_menu(TO_BOTTOM_MENU, mdata);	/* Abort up to the menu bar?	*/
	else if (choice == -1) verase_menu(ALL_MENUS, mdata);					/* Erase all.			*/
	else if (choice == 0)									/* Going up one level?		*/
	{
		verase_menu(TOP_MENU, mdata);						/* Yes, then erase the top menu.	*/
		goto up;								/* Continue processing.			*/
	}

	mdata->type = save_t;								/* Restore the menu type as it was.	*/
	mdata->column = save_c;
	mdata->width = save_w;
	vmove(svcurlin,svcurcol);							/* Restore basic status.		*/
	VL_vmode(svcurmod);
	vcharset(svchrset);
	return(choice);									/* Return the pointer.			*/
}

static int4 vmcont(struct video_menu *mdata)						/* Continuation search routine.		*/
{
	int4 choice;									/* Continuation selection.		*/
	if (mdata->path != NULL) choice = vmcont(mdata->path);				/* Recurse until back at the top.	*/
	else choice = vmenuvector(1, mdata);						/* Select the correct menu.		*/
	return(choice);									/* Return the choice.			*/
}

void VL_vdynalink(struct video_menu *mdroot, struct video_menu *mdlink)			/* Dynamically continue processing.	*/
{
	if (mdroot->path != NULL) VL_vdynalink(mdroot->path, mdlink);			/* Link forward from the last level.	*/
	else										/* Found where we are so...		*/
	{
		mdroot->link[mdroot->item] = mdlink;					/* Link in the control block.		*/
		mdlink->backlink = mdroot;						/* Remember where we come from.		*/
		mdlink->backitem = mdroot->item;					/* Remember which item we come from.	*/
		metapush = return_key;							/* Pretend a return was depressed.	*/
	}
}

void VL_vdynaunlink(struct video_menu *md)							/* Unlink a dynamic linked structure.	*/
{
	if (md->backlink != NULL)							/* Is there a backlink?			*/
	{
		disconnect(md->backlink, md->backitem);					/* Yes, so disconnect from it.		*/
		md->backitem = 0;							/* Now zero it out.			*/
		md->backlink = NULL;							/* No more backlink.			*/
	}
	verase_menu(ALL_MENUS, md);							/* Erase all of the menus.		*/
}

static int disconnect(struct video_menu *md,int item)					/* Disconnect paths and links.		*/
{
	md->link[item] = DYNAMIC_LINK;							/* Now it is a dynamic link again.	*/
/*	md->path = NULL;	*/							/* Now there isn't a path either.	*/
	return(SUCCESS);
}

static int4 vmenuvector(int state, struct video_menu *mdata)				/* Vector the correct menu type.	*/
{
	int4 choice = 0;									/* Selection made.			*/
	int4 save_t, save_c, save_w;							/* Menu type save location.		*/

	save_c = mdata->column;
	save_w = mdata->width;
	if ( !(save_t = mdata->type)) 							/* Assume a bar menu if not given.	*/
	{
		mdata->type = BAR_MENU;							/* Set the default type.		*/
		if (mdata->column == 0)							/* Set the default values.		*/
		{
			mdata->column = 2;
			mdata->width = 3;
		}
	}

	switch(mdata->type)								/* Select the menu type.		*/
	{
		case LOAD_FROM_FILE:	{ choice = vlff(state,"Not Specified"); break; }/* Load menu from a file?		*/
		case BAR_MENU:		{ choice = vbar(state,mdata); break; }		/* A bar menu?				*/
		case PULL_DOWN_MENU: 							/* A pull-down menu?			*/
		case POP_UP_MENU:	{ choice = vmx(state,mdata); break; }		/* A pop-up menu?			*/
		case NUMERIC_MENU:
		case ALPHABETIC_MENU:
		case HIGHLIGHTED_MENU:
		case DISPLAY_ONLY_MENU:	{ choice = vdo(state,mdata); break; }		/* A display only menu?			*/
		default:	{ vre("vmenuvector(%d)-Invalid menu type specified.",mdata->type); break; }
	}

	mdata->type = save_t;								/* Restore the menu type as it was.	*/
	mdata->column = save_c;
	mdata->width = save_w;
	return(choice);									/* Return the user's choice.		*/
}

/*						Menu bar menu.									*/

static int4 vbar(int state, struct video_menu *md)					/* Do a bar menu.			*/
{
	char barbuf[MAX_COLUMNS_PER_LINE+1];						/* Working buffer.			*/
	char temp[MAX_COLUMNS_PER_LINE+1];						/* Temporary storage.			*/
	int i,j,k;									/* Working registers.			*/
	int4 j4;
	unsigned char *vsss();								/* Pointer to the section save routine.	*/
	int item;									/* Menu choice.				*/
	int active;									/* Active menu flag.			*/
	int loc[MAX_MENU_ITEMS];							/* Item location.			*/
	int4 choice;									/* User selection.			*/
	int bmode, imode;								/* Display mode.			*/
	struct video_menu *next;							/* Pointer to next menu.		*/
	int found;									/* Found string flag.			*/

	choice = 0;									/* No choice yet.			*/

	if (state == 0)									/* Menu startup?			*/
	{
		if (md->save == NULL) md->save = vsss(0,0,1,VL_vscr_wid);			/* Save the first line.			*/
		else
		{
			vre("VL_vmenugo-F-Menu control block already in use and cannot be reused.");	/* Report in use.	*/
			return(FAILURE);								/* Don't continue.	*/
		}
	}

	for (i = 0; i < MAX_MENU_ITEMS; i++) loc[i] = -1;				/* Assume no items.			*/
	for (j = 0; j < VL_vscr_wid; j++) barbuf[j] = ' ';					/* Fill the buffer with spaces.		*/
	barbuf[j] = 0;									/* Store a trailing null.		*/

	for (i = 0, j = (int) md->column; (i < (int)md->items) && (j < VL_vscr_wid); i++)	/* Initialize the menu buffer.		*/
	{
		loc[i] = j;								/* Remember the position of this item.	*/
		tx(temp,md->text[i]);							/* Set the string to a working value.	*/
		for (k = 0; (temp[k] != 0) && (j < VL_vscr_wid); k++) barbuf[j++] = temp[k];	/* Copy the items text.		*/
		for (k = 0; (k < md->width) && (j < VL_vscr_wid); k++) barbuf[j++] = ' ';	/* Add appropriate spacing.		*/
	}

	bmode = VL_vmaskm(md->options);							/* Get the desired background mode.	*/
	imode = VMODE_REVERSE;								/* Assume item displayed in reverse.	*/
	adj(&bmode,&imode);								/* Adjust the modes.			*/

	item = (int) md->item;								/* Start where directed.		*/
	base_lin = vcur_lin;								/* Remember the base position.		*/
	base_col = vcur_col;

	VL_vbuffering_start();							/* Start of a logical section.		*/
	vmove(0,0);									/* Move to the menu bar line.		*/
	vcharset(DEFAULT);								/* Default characters.			*/
	vset_cursor_off();								/* Turn the cursor off.			*/
	for (i = 0; i < VL_vscr_wid; i++)							/* Loop through the full width.		*/
	{
		if (loc[item] == i)							/* Are we on the field to be selected?	*/
		{
			VL_vmode(imode);							/* Switch to black background.		*/
			k = lx(md->text[item]);						/* Get the length of the field.		*/
			for (j = 0; j < k; j++) vputc(barbuf[i+j]);			/* Output the selected function.	*/
			i = i + k - 1;							/* Move past this field.		*/
		}
		else									/* Just the background.			*/
		{
			VL_vmode(bmode);							/* Set the default menu bar.		*/
			vputc(barbuf[i]);						/* Output spaces.			*/
		}
	}
	VL_vbuffering_end();								/* End of logical section.		*/

	active = TRUE;									/* Menu is now active.			*/
	while (active)									/* Repeat while we are active.		*/
	{
		if ((k = metapush)) metapush = 0;						/* Get a pushed back character.		*/
		else if ((k = vmrest()))							/* Are we restoring the menu.		*/
		{
			vmove(0,loc[item]);						/* Move to the start of the field.	*/
			VL_vmode(bmode);							/* Select for blank out.		*/
			j = lx(md->text[item]) + loc[item];				/* Determine length of the field.	*/
			for (i = loc[item]; i < j; i++) vputc(barbuf[i]);		/* Output the reverse overtop.		*/
			item = k - 1;							/* Select the item.			*/
			vmove(0,loc[item]);						/* Move to the start of the field.	*/
			VL_vmode(imode);							/* Select for blank out.		*/
			j = lx(md->text[item]) + loc[item];				/* Determine length of the field.	*/
			for (i = loc[item]; i < j; i++) vputc(barbuf[i]);		/* Output the text.			*/
			k = return_key;							/* Make the selection.			*/
		}
		else									/* Actually get a character.		*/
		{
			vmove(0,VL_vscr_wid-1);						/* Move cursor to a pleasant place.	*/
			k = vgetm();							/* Get a character.			*/
		}

		VL_vbuffering_start();						/* Start of logical section.		*/
		if ((k == return_key) || (k == enter_key)) 				/* A return key?			*/
		{
			if ((md->link[item] == NULL) || (md->link[item] == DYNAMIC_LINK))	/* Is this a direct choice?	*/
			{
				choice = md->code[item];				/* Flag what was chosen.		*/
				md->item = item;					/* Remember where for next time.	*/
				md->path = NULL;					/* End of the path.			*/
				active = FALSE;						/* Now return to the caller.		*/
			}
			else metapush = down_arrow_key;					/* No, then make it work like down.	*/
		}

		else if (k == help_key) givehelp();					/* Give help?				*/

		else if (pfkeys && VL_vfnkey(k))						/* A function key?			*/
		{
			choice = md->code[item] + (VL_vfnkey(k) * 100000);			/* Yes, then code the functon too.	*/
			md->item = item;						/* No item either.			*/
			md->path = NULL;						/* Not going any where.			*/
			active = FALSE;							/* All done.				*/
		}

		else if (!pfkeys && (k == fn16_key))					/* Abort?				*/
		{
			choice = -1;							/* Yes, then code the functon too.	*/
			md->item = 0;							/* No item either.			*/
			md->path = NULL;						/* Not going any where.			*/
			active = FALSE;							/* All done.				*/
		}

		else if (k == down_arrow_key)						/* A down arrow?			*/
		{
			if ((next = md->link[item]) == NULL) metapush = return_key;	/* Any selection?			*/
			else if (md->link[item] == DYNAMIC_LINK) metapush = return_key;	/* Dynamic, so emulate it.		*/
			else								/* Yes, then menu again.		*/
			{
				int4 sv_type = 0;
				int4 sv_options = 0;
				int4 sv_row = 0;
				int4 sv_column = 0;		/* Save next parameters storage.	*/
				if (menumode == STATIC_MENU)				/* Save only if static.			*/
				{
					sv_type = next->type;				/* Save parameters we might change.	*/
					sv_options = next->options;
					sv_row = next->row;
					sv_column = next->column;
				}

				if (!next->type) next->type = PULL_DOWN_MENU;  		/* If not given, next is pull down.	*/
				if (!next->row) next->row = 1;				/* Is a row specified for next menu?	*/
				if (!next->column)					/* Is a column specified.		*/
				{
					next->column = loc[item];			/* Try to put it right under the bar.	*/
					while ((next->column + next->width + 2) > VL_vscr_wid) next->column--;
				}
				if (! ((next->options & LEFT_HANDED) && (next->options & RIGHT_HANDED)))	/* Handedness?	*/
				{
					if ((next->column + next->width) > (VL_vscr_wid - (VL_vscr_wid/4)))	/* Make right handed?	*/
					{
						next->options = next->options | LEFT_HANDED;		/* No, then go left.	*/
					}
					else next->options = next->options | RIGHT_HANDED;		/* Yes, go right.	*/
				}

				next->item = 0;						/* We are chaining down.		*/
				vmove(0,loc[item]);					/* Move to the start of the field.	*/
				md->path = next;					/* Remember the path.			*/
				j4 = VL_vmenugo(md->link[item]);				/* Call the menu.			*/

				if (menumode == STATIC_MENU)				/* Restore only if static.		*/
				{
					next->type = sv_type;				/* Restore parameters we changed.	*/
					next->options = sv_options;
					next->row = sv_row;
					next->column = sv_column;
				}

				if (!pfkeys && (j4 == -1)) j4 = 0;			/* Don't abort past the bar.		*/
				if (j4)							/* Any selection?			*/
				{
					choice = j4;					/* Add in the choice level.		*/
					md->item = item;				/* Remember the last selected item.	*/
					active = FALSE;					/* And we're all done.			*/
				}
				else							/* Didn't make a selection.		*/
				{
					md->path = NULL;				/* No path above us now.		*/
					vmove(0,loc[item]);				/* Move to the start of the field.	*/
					VL_vmode(imode);					/* Select for blank out.		*/
					j = lx(md->text[item]) + loc[item];		/* Determine length of the field.	*/
					for (i = loc[item]; i < j; i++) 		/* Reshow the selection.		*/
					{
						vputc(barbuf[i]);			/* Output data.				*/
					}
				}
			}
		}

		else if ((k == right_arrow_key) || (k == ' ') || (k == tab_key))	/* Move to the right?			*/
		{
			vmove(0,loc[item]);						/* Move to the start of the field.	*/
			VL_vmode(bmode);							/* Select for blank out.		*/
			j = lx(md->text[item]) + loc[item];				/* Determine length of the field.	*/
			for (i = loc[item]; i < j; i++) vputc(barbuf[i]);		/* Output the reverse overtop.		*/
tx1:			item++;								/* Count the choice.			*/
			if (loc[item] < 0) item = 0;					/* Wrap around?				*/
			tx(temp,md->text[item]);					/* Create a working string.		*/
			if (temp[0] == 0) goto tx1;					/* Empty field?				*/
			vmove(0,loc[item]);						/* Move forward.			*/
			VL_vmode(imode);							/* Switch to black background.		*/
			j = lx(md->text[item]) + loc[item];				/* Determine length of the field.	*/
			for (i = loc[item]; i < j; i++) vputc(barbuf[i]);		/* Output the selected function.	*/
		}

		else if (k == left_arrow_key)						/* Left arrow?				*/
		{
			vmove(0,loc[item]);						/* Move to the start of the field.	*/
			VL_vmode(bmode);							/* Select for blank out.		*/
			j = lx(md->text[item]) + loc[item];				/* Determine length of the field.	*/
			for (i = loc[item]; i < j; i++) vputc(barbuf[i]);		/* Output the reverse overtop.		*/
tx2:			item--;								/* Count the choice.			*/
			if (item < 0) item = MAX_MENU_ITEMS-1;				/* Wrap around?				*/
			while(loc[item] < 0) item--;					/* Figure out where the field is.	*/
			tx(temp,md->text[item]);					/* Make working copy.			*/
			if (temp[0] == 0) goto tx2;					/* Empty field?				*/
			vmove(0,loc[item]);						/* Move forward.			*/
			VL_vmode(imode);							/* Switch to black background.		*/
			j = lx(md->text[item]) + loc[item];				/* Determine length of the field.	*/
			for (i = loc[item]; i < j; i++) vputc(barbuf[i]);		/* Output the selected function.	*/
		}

		else if ((k > 040) && (k < 0177))					/* Is it a printing character?		*/
		{
			found = FALSE;							/* Assume not found.			*/
			i = item+1;							/* Move to the next item.		*/
			if (loc[i] < 0) i = 0;						/* Wrap around?				*/
			while (!found && (i != item))					/* Loop around looking for the flag.	*/
			{
				j = k;							/* Get a value of the letter.		*/
				if (islower((int)md->text[i][0])) j = tolower(k);		/* Make sure the cases match.		*/
				if (isupper((int)md->text[i][0])) j = toupper(k);
				if (md->text[i][0] == j) found = TRUE;			/* Does this match?			*/
				else
				{
					i++;						/* No, then try the next field.		*/
					if (loc[i] < 0) i = 0;				/* Yes, then wrap around.		*/
				}
			}
			if (found)							/* Was it found?			*/
			{
				vmove(0,loc[item]);					/* Move to the start of the field.	*/
				VL_vmode(bmode);						/* Select for blank out.		*/
				j = lx(md->text[item]) + loc[item];			/* Determine length of the field.	*/
				for (k = loc[item]; k < j; k++) vputc(barbuf[k]);	/* Output the reverse overtop.		*/
				item = i;						/* Select the new item.			*/
				vmove(0,loc[item]);					/* Move to the start of the field.	*/
				VL_vmode(imode);						/* Select for blank out.		*/
				j = lx(md->text[item]) + loc[item];			/* Determine length of the field.	*/
				for (k = loc[item]; k < j; k++) vputc(barbuf[k]);	/* Output the text.			*/
			}
			else vbell();							/* Not found so just beep.		*/
		}
		else vbell();								/* Anything else is invalid.		*/
		VL_vbuffering_end();							/* End of logical section.		*/
	}

	vmove(0,0);									/* Set some courtesy values.		*/
	VL_vmode(CLEAR);
	vset_cursor_on();								/* Restore the cursor.			*/
	if ((menumode == STATIC_MENU) || (choice == 0) || (choice == -1))		/* Eliminate the menu?			*/
	{
		vrss(md->save);								/* Restore the screen as it was.	*/
		md->save = NULL;							/* Make menu control block available.	*/
	}
	base_lin = -1;									/* Reset the current column.		*/
	base_col = -1;
	return(choice);									/* Return to the caller.		*/
}
/*						Pop-up/Pull-down Menus.								*/

static int4 vmx(int state, struct video_menu *md)					/* Pull down menu processing.		*/
{
	int i,j,k;									/* Working registers.			*/
	int row,col,rows,cols;								/* Window atributes.			*/
	unsigned char *vsss();								/* Window save area.			*/
	int4 j4, choice;								/* Selection made.			*/
	int item;									/* Item being worked on.		*/
	int active;									/* Active input flag.			*/
	int bmode,imode;								/* Display renditions.			*/
	struct video_menu *next;							/* Next menu item.			*/
	int found;									/* Flagging variable.			*/
	int4 save_row, save_column;							/* More save words.			*/
	int linking;									/* Linking detection flag.		*/

	choice = 0;									/* No choice yet.			*/
	item = (int) md->item;								/* Start on the requested item.		*/

	save_column = md->column;							/* Save the position data.		*/
	if (!(save_row = md->row))
	{
		detpos((int)md->options,&md->row,&md->column,(int)md->width+1,(int)md->items);		/* Is position given?	*/
	}
	if (md->type == PULL_DOWN_MENU)							/* Pull downs menu?			*/
	{
		row = 1;								/* Pull downs are on the second line.	*/
		rows = (int) md->items+1;						/* Need items+1 lines.			*/
	}
	else										/* No, then it is a pop-up menu.	*/
	{
		row = (int) md->row;							/* Use an actual row.			*/
		rows = (int) md->items+2;						/* Need items+2 lines.			*/
	}
	col = (int) md->column-1;							/* Determine the column.		*/
	if (col < 0) col = 0;								/* Don't go off the screen.		*/
	cols = (int) md->width+3;							/* Leave room for the border.		*/

	bmode = VL_vmaskm(md->options);							/* Get the desired background mode.	*/
	imode = VMODE_REVERSE;								/* Assume item displayed in reverse.	*/
	adj(&bmode,&imode);								/* Adjust for screen background.	*/

	VL_vbuffering_start();							/* Start of logical section.		*/

	if (state == 0)									/* First time?				*/
	{
		if (md->save == NULL) md->save = vsss(row-1,col,rows+1,cols);		/* Save what is below this menu.	*/
		else
		{
			vre("vmenu-F-Menu control block already in use an cannot be reused.");	/* Report already in use.	*/
			return(FAILURE);							/* Don't continue.		*/
		}
	}

	vset_cursor_off();								/* Turn the cursor off.			*/
	VL_vmode(bmode);									/* No renditions.			*/
	vcharset(0);									/* Default character set.		*/
	VL_varb(md->row,md->column,md->items,md->width+1);				/* Invalidate the map (WATCH OUT)!!	*/

	linking = 0;									/* Assume no links to other menus.	*/
	for (i = 0; i < md->items; i++)							/* Loop through each item.		*/
	{
		if (md->link[i]) linking = 2;						/* Flag the linking factor.		*/
	}

	for (i = 0; i < md->items; i++)							/* Loop through each item.		*/
	{
		vmove(row+i,md->column);						/* Move to the location.		*/
		if (i == item) VL_vmode(imode);						/* Select correct rendition.		*/
		else VL_vmode(bmode);							/* Background mode.			*/
		padout2(md->text[i],(int)md->width,linking,(int)md->options,md->link[i]);	/* Output the text.		*/
	}

	if (md->type == POP_UP_MENU)							/* Output pop-up menu frame.		*/
	{
		vmove(row-1,col);
		vline(VERTICAL,rows);
		vmove(row-1,col+cols-1);
		vline(VERTICAL,rows);
		vmove(row-1,col);
		vline(HORIZONTAL,cols);
		vmove(row+rows-2,col);
		vline(HORIZONTAL,cols);
	}
	else
	{
		vmove(row,col);
		vline(VERTICAL,rows);
		vmove(row,col+cols-1);
		vline(VERTICAL,rows);
		vmove(row+rows-1,col);
		vline(HORIZONTAL,cols);
	}

#ifdef OLD
	linking = 0;									/* Assume no links to other menus.	*/
	for (i = 0; i < md->items; i++)							/* Loop through each item.		*/
	{
		if (md->link[i]) linking = 2;						/* Flag the linking factor.		*/
	}

	for (i = 0; i < md->items; i++)							/* Loop through each item.		*/
	{
		vmove(row+i,md->column);						/* Move to the location.		*/
		if (i == item) VL_vmode(imode);						/* Select correct rendition.		*/
		else VL_vmode(bmode);							/* Background mode.			*/
		padout2(md->text[i],md->width,linking,md->options,md->link[i]);		/* Output the text.			*/
	}
#endif

	VL_vbuffering_end();								/* End of logical section.		*/

	active = TRUE;									/* Menu is now active.			*/
	while (active)									/* Repeat while we are active.		*/
	{
		if ((k = metapush)) metapush = 0;						/* Get a pushed back character.		*/
		else if ((k = vmrest()))							/* Are we restoring the menu.		*/
		{
			vmove(row+item,md->column);					/* Move to the current item.		*/
			VL_vmode(bmode);							/* Remove the current selection.	*/
			padout2(md->text[item],(int)md->width,linking,(int)md->options,md->link[item]);
			item = k - 1;							/* Select the item.			*/
			vmove(row+item,md->column);					/* Move to the current item.		*/
			VL_vmode(imode);							/* Remove the current selection.	*/
			padout2(md->text[item],(int)md->width,linking,(int)md->options,md->link[item]);
			k = return_key;							/* Make the selection.			*/
		}
		else if ((k = metapush)) metapush = 0;					/* Get a pushed back character.		*/
		else									/* Get a character.			*/
		{
			vmove(0,VL_vscr_wid-1);						/* Move to alternate home position.	*/
			k = vgetm();							/* Get a character.			*/
		}

		VL_vbuffering_start();						/* Start of logical section.		*/

		if ((k == return_key) || (k == enter_key)) 				/* A return key?			*/
		{
			if ((md->link[item] == NULL) || (md->link[item] == DYNAMIC_LINK))	/* Is this a direct choice?	*/
			{
				choice = md->code[item];				/* Flag what was chosen.		*/
				md->item = item;					/* Remember the item.			*/
				md->path = NULL;					/* No path from here.			*/
				active = FALSE;						/* Now return to the caller.		*/
			}
			else metapush = right_arrow_key;				/* No, then make it work like right.	*/
		}

		else if (k == help_key) givehelp();					/* Give helpfull information.		*/

		else if (pfkeys && VL_vfnkey(k))						/* A function key?			*/
		{
			choice = md->code[item] + (VL_vfnkey(k) * 100000);			/* Yes, so no choice.			*/
			md->item = item;						/* Remember the item.			*/
			md->path = NULL;						/* No path from here.			*/
			active = FALSE;							/* All done.				*/
		}

		else if (!pfkeys && (k == fn16_key))					/* Abort?				*/
		{
			choice = -1;							/* Yes, so no choice.			*/
			md->item = 0;							/* Remember the item.			*/
			md->path = NULL;						/* No path from here.			*/
			active = FALSE;							/* All done.				*/
		}

		else if ((k == right_arrow_key) || (k == left_arrow_key))		/* A down arrow?			*/
		{
			j = left_arrow_key;						/* Assume an exit left window.		*/
			if (md->options & EXIT_RIGHT) j = right_arrow_key;		/* Was it exit right?			*/
			if ((item == 0) && (k == j))					/* Abort from the menu?			*/
			{
				choice = 0;						/* No choice made.			*/
				md->item = 0;						/* First item is now first.		*/
				md->path = NULL;					/* No path from here.			*/
				active = FALSE;						/* No longer an active menu.		*/
			}
			else if (md->link[item] == NULL) metapush = return_key; 	/* Right arrow?				*/
			else if (md->link[item] == DYNAMIC_LINK) metapush = return_key;	/* Make it return a value.		*/
			else								/* Yes, then call vmenu again.		*/
			{
				int4 sv_type = 0;
				int4 sv_options = 0;
				int4 sv_row = 0;
				int4 sv_column = 0;		/* Save next parameters storage.	*/

				next = md->link[item];					/* Point to the next item.		*/
				if (menumode == STATIC_MENU)				/* Save?				*/
				{
					sv_type = next->type;				/* Save parameters we might change.	*/
					sv_options = next->options;
					sv_row = next->row;
					sv_column = next->column;
				}

				if (!next->type) next->type = POP_UP_MENU;		/* If not given, next type is pop-up.	*/
				if (!next->row)						/* Is a row specified for next menu?	*/
				{
					next->row = md->row + item + 2;			/* Always try to drop two rows.		*/
					while (next->row >= md->row + md->items) next->row = next->row - 1;	/* Not too far.	*/
					if (next->row < 2) next->row = 2;		/* Don't go off the top.		*/
					while ((next->row + next->items + 2) > MAX_LINES_PER_SCREEN) next->row--;
				}
				if (!next->column)					/* Is a column specified.		*/
				{
					next->column = md->column + md->width - ((md->width/4)+1);
					if (md->options & LEFT_HANDED) next->column = md->column - next->width-1 + ((md->width/4));
					if (next->column < 2) next->column = 2;
					while ((next->column + next->width + 2) > VL_vscr_wid) next->column--;
				}
				if (! ((next->options & LEFT_HANDED) && (next->options & RIGHT_HANDED)))
				{
					if ((next->column + next->width) > (VL_vscr_wid-(VL_vscr_wid/4)))
					{
						next->options = next->options | LEFT_HANDED;		/* No, then go left.	*/
					}
					else next->options = next->options | RIGHT_HANDED;		/* Yes, go right.	*/
				}

				next->item = 0;						/* Chaining down so start on 1st line.	*/
				vmove(row+item,md->column);				/* Move to the current item.		*/
				md->path = next;					/* Remember where we went.		*/
				j4 = VL_vmenugo(md->link[item]);				/* Call the menu.			*/

				if (menumode == STATIC_MENU)				/* Restore?				*/
				{
					next->type = sv_type;				/* Restore changed parameters.		*/
					next->options = sv_options;
					next->row = sv_row;
					next->column = sv_column;
				}

				if (j4)							/* Any selection?			*/
				{
					choice = j4;					/* Add in the choice level.		*/
					md->item = item;				/* Remember the item.			*/
					active = FALSE;					/* And we're all done.			*/
				}
				else							/* Didn't make a selection.		*/
				{
					md->path = NULL;				/* No path above now.			*/
					vmove(row+item,md->column);			/* Move to the current item.		*/
					VL_vmode(imode);					/* Select for blank out.		*/
					padout2(md->text[item],(int)md->width,linking,(int)md->options,md->link[item]);
				}
			}
		}

		else if ((k == down_arrow_key) || (k == ' '))				/* A down arrow?			*/
		{
			vmove(row+item,md->column);					/* Move to the current item.		*/
			VL_vmode(bmode);							/* Remove the current selection.	*/
			padout2(md->text[item],(int)md->width,linking,(int)md->options,md->link[item]);
tx3:			item++;								/* Select the next item.		*/
			if (item >= md->items) item = 0;				/* Wrap around if necessary.		*/
			if (md->text[item][0] == 0) goto tx3;				/* Empty field?				*/
			vmove(row+item,md->column);					/* Move to the current item.		*/
			VL_vmode(imode);							/* Remove the current selection.	*/
			padout2(md->text[item],(int)md->width,linking,(int)md->options,md->link[item]);
		}

		else if (k == up_arrow_key)						/* Move up?				*/
		{
			if (item == 0) 							/* Abort from the menu?			*/
			{
				choice = 0;						/* No choice made.			*/
				md->item = 0;						/* First item is now first.		*/
				md->path = NULL;					/* No path.				*/
				active = FALSE;						/* No longer an active menu.		*/
			}
			else								/* Didn't go off the top so.		*/
			{
				vmove(row+item,md->column);				/* Move to the current item.		*/
				VL_vmode(bmode);						/* Remove the current selection.	*/
				padout2(md->text[item],(int)md->width,linking,(int)md->options,md->link[item]);
tx4:				item--;							/* Select the next item.		*/
				if (item < 0) item = (int) md->items-1;			/* Wrap around if necessary.		*/
				if (md->text[item][0] == 0) goto tx4;			/* Empty field?				*/
				vmove(row+item,md->column);				/* Move to the current item.		*/
				VL_vmode(imode);						/* Remove the current selection.	*/
				padout2(md->text[item],(int)md->width,linking,(int)md->options,md->link[item]);
			}
		}

		else if ((k > 040) && (k < 0177))					/* Is it a printing character?		*/
		{
			found = FALSE;							/* Assume not found.			*/
			i = item+1;							/* Move to the next item.		*/
			if (i >= md->items) i = 0;					/* Wrap around?				*/
			while (!found && (i != item))					/* Loop around looking for the flag.	*/
			{
				j = k;							/* Make sure j isn't 0.			*/
				if (islower((int)md->text[i][0])) j = tolower(k);		/* Make sure the cases match.		*/
				if (isupper((int)md->text[i][0])) j = toupper(k);
				if (md->text[i][0] == j) found = TRUE;			/* Does this match?			*/
				else
				{
					i++;						/* No, then try the next field.		*/
					if (i >= md->items) i = 0;			/* Yes, then wrap around.		*/
				}
			}
			if (found)							/* Was it found?			*/
			{
				vmove(row+item,md->column);				/* Move to the current item.		*/
				VL_vmode(bmode);						/* Remove the current selection.	*/
				padout2(md->text[item],(int)md->width,linking,(int)md->options,md->link[item]);
				item = i;						/* Select the item.			*/
				vmove(row+item,md->column);				/* Move to the current item.		*/
				VL_vmode(imode);						/* Remove the current selection.	*/
				padout2(md->text[item],(int)md->width,linking,(int)md->options,md->link[item]);
			}
			else vbell();							/* Not found so just beep.		*/
		}

		else vbell();								/* Anything else is invalid.		*/

		VL_vbuffering_end();							/* End of logical section.		*/
	}

	md->row = save_row;								/* Restore the initial positon.		*/
	md->column = save_column;
	if ((menumode == STATIC_MENU) || (choice == 0) || (choice == -1))		/* Auto eliminate the menu?		*/
	{
		vrss(md->save);								/* Restore the screen as it was.	*/
		md->save = NULL;							/* Make menu control block available.	*/
	}
	return(choice);									/* Return the choice.			*/
}

static int padout(char *text, int width, int do_line)					/* Output padded text.			*/
{
	register int i;									/* Working register.			*/
	char buffer[MAX_COLUMNS_PER_LINE];						/* Working buffer.			*/

	VL_vbuffering_start();							/* Start of logical section.		*/
	tx(buffer,text);								/* Get a terminated copy.		*/

	if (buffer[0] == '\0' && do_line)						/* Anything to output?			*/
	{
		vslew(0,-1);								/* Slew back.				*/
		vline(HORIZONTAL, width+2);						/* Output a line.			*/
		vslew(0,-1);								/* Back to end of position.		*/
		vcharset(DEFAULT);							/* Reset the character set.		*/
	}
	else
	{
		for (i = 0; i < width; i++) buffer[i] = ' ';				/* Loop through each character.		*/
		buffer[i] = 0;								/* Store an ending null.		*/

		for (i = 0; (text[i] != 0) && (i < width); i++) buffer[i] = text[i];	/* Move in the data.			*/
		vprint("%s",buffer);							/* Now print it.			*/
	}

	VL_vbuffering_end();								/* End of logical section.		*/

	return(SUCCESS);
}
/*						Load a menu from a file.							*/

static int vlff(int state, char *file)							/* Load menu data from file.		*/
{
	int choice = 0;									/* Chosen selection.			*/
	vtext(BOLD,10,10,"Loading a menu from file %s is not implemented yet, sorry!",file);
	return(choice);
}

/*						Initialize a menu.								*/

int VL_vmenuinit(struct video_menu *md, int t, int o, int r, int c, int w)			/* Get the menu data.			*/
{
	int i;										/* Working registers.			*/
	switch(t)									/* Validate the menu type.		*/
	{
		case LOAD_FROM_FILE: return(FAILURE);					/* Load menu from a file?		*/
		case UNKNOWN_MENU:
		case DISPLAY_ONLY_MENU:
		case BAR_MENU:
		case PULL_DOWN_MENU:
		case POP_UP_MENU:	break;						/* Above this is ok.			*/
		default:	{ vre("VL_vmenuinit(%d)-Invalid menu type specified.", t); return(FAILURE); }
	}

	md->backitem = 0;								/* No backitem.				*/
	md->backlink = NULL;								/* No backlink.				*/
	md->path = NULL;								/* No selected linkage yet.		*/
	md->item = 0;									/* And no back item yet.		*/
	md->save = NULL;								/* And memory not saved yet.		*/
	md->type = t;									/* Initialize the type of menu.		*/
	md->options = o;								/* Initialize the options.		*/
	if ((r) && ((r < 0) || (r >= MAX_LINES_PER_SCREEN-2))) 				/* Valid row specified?			*/
	{
		vre("VL_vmenuinit(%d)-Invalid initial row specified.",r);			/* Report the error.			*/
		return(FAILURE);							/* Cannot continue.			*/
	}
	md->row = r;									/* Load the row.			*/

	if ((c) && ((c < 1) || (c > MAX_COLUMNS_PER_LINE-2)))				/* Valid column?			*/
	{
		vre("VL_vmenuinit(%d)-Invalid initial column specified.",c);		/* Report the error.			*/
		return(FAILURE);							/* Don't continue.			*/
	}

	md->column = c;									/* Load the column.			*/

	if (w && ((w < 1) || (w > MAX_MENU_WIDTH)))					/* In bounds?				*/
	{
		vre("VL_vmenuinit(%d)-Invalid initial width specified.",w);		/* Report the error.			*/
		return(FAILURE);
	}
	else if (w) md->width = w;							/* User gave a valid w.			*/
	else md->width = 1;								/* Never let less than 1.		*/

	for (i = 0; i < MAX_MENU_ITEMS; i++)						/* Now clear out the item data.		*/
	{
		md->link[i] = NULL;							/* Set all links to null.		*/
		md->code[i] = 0;							/* Set all codes to zero.		*/
		md->text[i][0] = 0;							/* Set all text to null.		*/
	}

	md->items = 0;									/* No items yet entered.		*/
	return(SUCCESS);								/* Return to the caller.		*/
}

/*						Fill in item data.								*/

int VL_vmenuitem(struct video_menu *md, char *s, int v, struct video_menu *nm)
{
	register int i,j,k;
	char temp[MAX_COLUMNS_PER_LINE];

	md->items++;									/* Count this addition.			*/
	if (md->items > MAX_MENU_ITEMS)
	{
		vre("VL_vmenuitem(%d)-This item is too many.",md->items);			/* Report the error.			*/
		return(FAILURE);
	}

	i = (int) md->items-1;								/* Select the item index.		*/

	j = lx(s);									/* Get the length of the string.	*/
	if (j >= MAX_MENU_WIDTH)							/* Will the string fit?			*/
	{
		for (k = 0; k < MAX_MENU_WIDTH-2; k++) md->text[i][k] = s[k];		/* Copy as much as we can.		*/
		md->text[i][k] = 0;							/* Put in the null terminator.		*/
		if (md->type != BAR_MENU) md->width = MAX_MENU_WIDTH;			/* As wide as we can go.		*/
	}
	else										/* Just use the width given.		*/
	{
		tx(temp,s);								/* Get a working copy.			*/
		strcpy((char *)md->text[i],(char *)temp);				/* Copy the whole string.		*/
		if ((md->type != BAR_MENU) && (j > md->width)) md->width = j;		/* Store it.				*/
	}

	md->code[i] = v;								/* Copy the item code.			*/
	md->link[i] = nm;								/* Link to the next menu.		*/
	
	return(SUCCESS);
}

static int tx(char *out, char *in) 							/* Setup a string for C usage.		*/
{
	register int k;									/* Working register.			*/
	for (k = 0; k < MAX_MENU_WIDTH; k++) out[k] = in[k];				/* Copy the string.			*/
	out[MAX_MENU_WIDTH-1] = 0;							/* Store a trailing null.		*/
	VL_vtrim(out);									/* Trim the string.			*/
	for (k = 0; k < MAX_MENU_WIDTH; k++)						/* Remove control characters.		*/
	{
		if (out[k] < 040) out[k] = 0;						/* Set funny characters to nulls.	*/
	}
	return 0;
}

static int lx(char *string)								/* Get the length of a string.		*/
{
	char temp[MAX_COLUMNS_PER_LINE];						/* Allocate storage.			*/

	tx(temp,string);								/* Trim the string.			*/
	return(strlen((char *)temp));							/* Return the length.			*/
}

static int adj(int *bmode, int *imode)							/* Change the mode to match background.	*/
{
	if (*bmode & VMODE_REVERSE) *imode = VMODE_BOLD;				/* Nope, then display item in bold.	*/
	if (vscr_atr & LIGHT) 								/* Add in bold if reverse screen.	*/
	{
		*bmode = VMODE_BOLD;							/* Funny opposites when reverse screen.	*/
		*imode = VMODE_REVERSE|VMODE_BOLD;
	}
	return 0;
}

int VL_vmenumode(int mode)									/* Change the menu mode.		*/
{
	menumode = mode;								/* Make the change.			*/
	return(SUCCESS);
}

/*						Display Only Menus.								*/

static int vdo(int state, struct video_menu *md)					/* Pull down menu processing.		*/
{
	register int i, k;								/* Working registers.			*/
	int row,col,rows,cols;								/* Window atributes.			*/
	unsigned char *vsss();								/* Window save area.			*/
	int bmode,imode;								/* Display renditions.			*/
	int4 save_row, save_column;							/* Save locations.			*/

	save_column = md->column;							/* Save the position data.		*/
	if (!(save_row = md->row))
	{
		detpos((int)md->options,&md->row,&md->column,(int)md->width,(int)md->items);		/* Is position given?	*/
	}
	row = (int) md->row;								/* Use an actual row.			*/
	rows = (int) md->items+2;							/* Need items+2 lines.			*/
	col = (int) md->column-1;							/* Determine the column.		*/
	if (col < 0) col = 0;								/* Don't go off the screen.		*/
	cols = (int) md->width+2;							/* Leave room for the border.		*/

	bmode = VL_vmaskm(md->options);							/* Get the desired background mode.	*/
	imode = VMODE_REVERSE;								/* Assume item displayed in reverse.	*/
	adj(&bmode,&imode);								/* Adjust for screen background.	*/

	VL_vbuffering_start();							/* Start of logical section.		*/

	if (state == 0)									/* First time?				*/
	{
move:		if (md->save == NULL) md->save = vsss(row-1,col,rows,cols);		/* Save what is below this menu.	*/
		else
		{
			vre("vmenu-F-Menu control block already in use an cannot be reused.");	/* Report already in use.	*/
			return(FAILURE);							/* Don't continue.		*/
		}
	}

	VL_vmode(bmode);									/* No renditions.			*/
	vcharset(DEFAULT);								/* Default character set.		*/
	for (i = 0; i < md->items; i++)							/* Loop through each item.		*/
	{
		vmove(row+i,col+1);							/* Move to the location.		*/
		padout(md->text[i],(int)md->width,0);					/* Output padded data.			*/
	}

	VL_vmode(bmode);									/* No renditions.			*/
	vmove(row-1,col);
	vline(VERTICAL,rows);
	vmove(row-1,col+cols-1);
	vline(VERTICAL,rows);
	vmove(row-1,col);								/* Output the grid.			*/
	vline(HORIZONTAL,cols);
	vmove(row+rows-2,col);
	vline(HORIZONTAL,cols);
	VL_vmode(bmode);									/* No renditions.			*/
	vcharset(DEFAULT);								/* Default character set.		*/

	if (VL_vdl_lin >= 0) vmove(VL_vdl_lin, VL_vdl_col);				/* Move to the specified cursor pos.	*/
	else vset_cursor_off();								/* Else set the cursor off.		*/
	VL_vmode(bmode);
	vcharset(DEFAULT);

	k = vgetm();									/* Get a character.			*/

	i = FALSE;									/* Assume we don't need to move.	*/
	if (k == left_arrow_key)							/* Left arrow?				*/
	{
		col = col - 8;
		if (col <= 0) col = 0;
		i = TRUE;
	}
	else if (k == right_arrow_key)
	{
		col = col + 8;
		if ((col + (int) md->width) >= VL_vscr_wid-2) col = VL_vscr_wid - (int) md->width - 2;
		i = TRUE;
	}
	else if (k == up_arrow_key)
	{
		row = row - 3;
		if (row < 1) row = 1;
		i = TRUE;
	}
	else if (k == down_arrow_key)
	{
		row = row + 3;
		if ((row + (int)md->items) >= MAX_LINES_PER_SCREEN - 1) row = MAX_LINES_PER_SCREEN - (int) md->items - 1;
		i = TRUE;
	}

	if (i)
	{
		vrss(md->save);
		md->save = NULL;
		goto move;
	}

	md->row = save_row;								/* Restore the initial positon.		*/
	md->column = save_column;
	vrss(md->save);									/* Restore the screen as it was.	*/
	md->save = NULL;								/* Make menu control block available.	*/

	if (VL_vdl_lin >= 0) VL_vdl_lin = -1;						/* Set the position invalid.		*/
	else vset_cursor_on();								/* Else put the cursor back on.		*/
	VL_vbuffering_end();								/* End of logical section.		*/
	return(k);									/* Return the choice.			*/
}

static int detpos(int op, int4 *row, int4 *col, int width, int items)			/* Determine the position for a window.	*/
{
	int temp;
	int r,c;
	
	if (op & CENTER_MENU)								/* Center the menu?			*/
	{
		*row = ((MAX_LINES_PER_SCREEN - (items+2)) / 2);
		*col = ((VL_vscr_wid - (width+2)) / 2);
		return(SUCCESS);
	}

	if ((*col < 1) || (*col > VL_vscr_wid-2)) VL_vdetpos(0, &temp, &c, items+2, width+2);		/* Is a column given?	*/
	if ((*row < 1) || (*row > MAX_LINES_PER_SCREEN)) VL_vdetpos(0, &r, &temp, items+2, width+2);	/* Is a row given?	*/
	*col = c;
	*row = r;
	return(SUCCESS);								/* Return to the caller.		*/
}

int VL_vdetpos(int scrpos, int *r, int *c, int rs, int cs)					/* Suggest a row and column.		*/
{
	int last_row, last_col;								/* Working variables.			*/
	int mlps;

	mlps = MAX_LINES_PER_SCREEN;

	if (!scrpos && ((last_row = base_lin) >= 0)) last_col = base_col;		/* Get the base cursor position.	*/
	else
	{
		last_row = vcur_lin;							/* Get the current row/column.		*/
		last_col = vcur_col;
	}

	*r = ((mlps - rs) / 2);								/* Center by default.			*/
	*c = ((VL_vscr_wid - cs) / 2);

	if ((last_row == 0) && (last_col == 0));					/* Home?				*/
	else
	{
		if (last_row < mlps/2)							/* In top 1/2 of screen?		*/
		{
			*r = ((mlps - rs) / 2) + (mlps / 2);
			while ((*r + rs) >= mlps) *r = *r - 1;				/* Normalize.				*/
		}
		else
		{
			*r = ((mlps - rs) / 2) - (mlps / 2);
			while (*r < 1) *r = *r + 1;					/* Normalize.				*/
		}

		if (last_col < VL_vscr_wid/2)						/* In left half of screen?		*/
		{
			*c = (VL_vscr_wid / 2) + ((VL_vscr_wid - cs) / 2);			/* Move to right half.			*/
			while ((*c + cs) >= VL_vscr_wid) *c = *c - 1;			/* Normalize.				*/
		}
		else
		{
			*c = ((VL_vscr_wid - cs) / 2) - (VL_vscr_wid / 2);
			while (*c < 0) *c = *c + 1;					/* Normalize.				*/
		}
	}
	return(SUCCESS);
}

int VL_vmenu_pfkeys(int state)
{
	if (state == ON) pfkeys = TRUE;
	else pfkeys = FALSE;
	return 0;
}

int VL_vmenustatus(int *r1, int *r2)
{
	*r1 = menumode;
	*r2 = pfkeys;
	return 0;
}

static int givehelp(void)
{
	struct video_menu help;

	VL_vmenuinit(&help,DISPLAY_ONLY_MENU,VMODE_REVERSE,0,0,0);
	VL_vmenuitem(&help,"How Pull Down Menus Work", 0, NULL);
	VL_vmenuitem(&help,"", 0, NULL);
	VL_vmenuitem(&help,"Navigate to the appropriate selection and depress return.", 0, NULL);
	VL_vmenuitem(&help,"The arrow keys, space bar and return can be used for this", 0, NULL);
	VL_vmenuitem(&help,"purpose.  In addition,  you can move to the desired field", 0, NULL);
	VL_vmenuitem(&help,"by entering the first letter of the field description.", 0, NULL);
	VL_vmenuitem(&help,"", 0, NULL);
	VL_vmenuitem(&help,"Depress any key to exit...", 0, NULL);

	VL_vmenugo(&help);
	vset_cursor_off();
	return(SUCCESS);
}

int VL_vmenusave(struct video_menu *md)							/* Save a menu's choice path.		*/
{
	FILE *fp;								/* File handling definitions.		*/

	fp = VL_vopenf("men","w");								/* Open a file for output.		*/

	fprintf(fp, "%d\n", md->item);							/* Write the data.			*/

	while (md->path != NULL) 							/* Loop until no further links.		*/
	{
		md = md->path;								/* Move to the next menu control block.	*/
		fprintf(fp, "%d\n", md->item);						/* Write the data.			*/
	}

	fclose(fp);									/* Close the file.			*/
	return(SUCCESS);								/* Return to the caller.		*/
}

int VL_vmenurestore(void) 									/* Restore a choice path.		*/
{
	restore = VL_vopenf("men", "r");							/* Open the file.			*/
	return(SUCCESS);								/* Return to the caller.		*/
}

static int vmrest(void)									/* Is a restore in progress?		*/
{
	int i;										/* Working variables.			*/

	if (restore == NULL) return(FAILURE);						/* Return failure if not active.	*/
	if (fscanf(restore, "%d", &i) == EOF)						/* Read the value.			*/
	{
		fclose(restore);							/* Close the file.			*/
		restore = NULL;								/* Set the pointer to null.		*/
		return(FAILURE);							/* Return an error.			*/
	}
	return(i+1);									/* Return the value.			*/
}

static int padout2(char *text, int width, int linking, int options, struct video_menu *link)
{
	char temp[MAX_COLUMNS_PER_LINE];						/* Temporary working string.		*/
	register int i;									/* Working registers.			*/

	temp[0] = CHAR_NULL;								/* Make sure the string is null.	*/

	if (!linking)									/* No linking so just output text.	*/
	{
		strcpy(temp,text);							/* Copy the string.			*/
		strcat(temp," ");
		padout(temp,width+1,1);							/* Output the description.		*/
	}

	else if (link)									/* Is this actually a link?		*/
	{
		if (options & LEFT_HANDED)						/* Left handed link?			*/
		{
			temp[0] = VL_vcapvalue(GRAPHSTR)[LEFT_POINTER];			/* Copy in a left pointer.		*/
			temp[1] = CHAR_NULL;						/* Keep the string null terminated.	*/
			if ((temp[0] == CHAR_NULL) || (temp[0] == '<'))			/* Is a pointer defined?		*/
			{
				strcpy(temp,"<");					/* No, then just output the < symbol.	*/
				strcat(temp,text);					/* Add the description.			*/
				padout(temp,width+1,1);					/* Output the description.		*/
			}
			else								/* Yes, then fix up the string.		*/
			{
				strcat(temp,text);					/* Add the text.			*/
				padout3(temp,width+1,1);				/* Output using graphics character.	*/
			}
		}
		else
		{
			strcpy(temp,text);						/* Copy in the string.			*/
			i = 0;
			while(temp[i] != CHAR_NULL) i++;				/* Loop to the end.			*/
			while(i <= width) temp[i++] = ' ';				/* Fill with spaces.			*/
			temp[i] = CHAR_NULL;						/* Terminate.				*/
			temp[--i] = VL_vcapvalue(GRAPHSTR)[RIGHT_POINTER];			/* Insert the pointer.			*/
			if ((temp[i] == CHAR_NULL) || (temp[i] == '>'))			/* Use a graphics version?		*/
			{
				temp[i] = '>';						/* No, insert the no-grapics pointer.	*/
				padout(temp,width+1,1);					/* Output the description.		*/
			}
			else padout3(temp,width+1,0);					/* Output using graphics character.	*/
		}
	}

	else
	{
		if (options & LEFT_HANDED)						/* Left or right handed?		*/
		{
			strcpy(temp," ");
			strcat(temp,text);
		}
		else
		{
			strcpy(temp,text);
			strcat(temp," ");
		}
		padout(temp,width+1,1);							/* Output the description.		*/
	}

	return(SUCCESS);
}

int VL_vlastlevel(struct video_menu *md)							/* Return the current path level.	*/
{
	struct video_menu *x;								/* Working pointer.			*/
	int i;										/* Working integers.			*/

	x = md;										/* Point to the root control block.	*/
	i = 0;										/* This is level 0.			*/
	while (x->path != NULL)								/* Loop until at the top level.		*/
	{
		x = x->path;								/* Point to the next level.		*/
		i++;									/* Count the level.			*/
	}
	return(i);									/* Return the level.			*/
}

int4 VL_vlastitem(struct video_menu *md)							/* Return the current path level.	*/
{
	struct video_menu *x;								/* Working pointer.			*/
	register int i;									/* Working integers.			*/

	x = md;										/* Point to the root control block.	*/
	i = 0;										/* This is level 0.			*/
	while (x->path != NULL)								/* Loop until at the top level.		*/
	{
		x = x->path;								/* Point to the next level.		*/
		i++;									/* Count the level.			*/
	}
	return(x->item);								/* Return this level's item.		*/
}

int VL_vlastlink(struct video_menu *md)							/* Get if the last selection links.	*/
{
	struct video_menu *x;								/* Working pointer.			*/
	register int i;									/* Working integers.			*/

	x = md;										/* Point to the root control block.	*/
	i = 0;										/* This is level 0.			*/
	while (x->path != NULL)								/* Loop until at the top level.		*/
	{
		x = x->path;								/* Point to the next level.		*/
		i++;									/* Count the level.			*/
	}
	if (x->link[x->item] == NULL) return(0);					/* No link.				*/
        else return(1);									/* Link.				*/
}

static int padout3(char *text, int width, int graphics_1st)				/* Output padded text.			*/
{
	register int i;									/* Working register.			*/
	char buffer[MAX_COLUMNS_PER_LINE];						/* Working buffer.			*/

	VL_vbuffering_start();							/* Start of logical section.		*/

	if (graphics_1st)								/* Output the graphics 1st?		*/
	{
		vcharset(GRAPHICS);							/* Turn on graphics mode.		*/
		vprint("%c",text[0]);							/* Output the pointer character.	*/
		vcharset(DEFAULT);							/* Put back to normal.			*/
		for (i = 0; i < width-1; i++) buffer[i] = ' ';				/* Loop through each character.		*/
		buffer[i] = 0;								/* Store an ending null.		*/
		for (i = 0; (text[i+1]!=0) && (i<width-1); i++) buffer[i] = text[i+1];	/* Move in the data.			*/
		vprint("%s",buffer);							/* Now print it.			*/
	}
	else
	{
		for (i = 0; i < width-1; i++) buffer[i] = ' ';				/* Loop through each character.		*/
		buffer[i] = 0;								/* Store an ending null.		*/
		for (i = 0; (text[i] != 0) && (i < width-1); i++) buffer[i] = text[i];	/* Move in the data.			*/
		vprint("%s",buffer);							/* Now print it.			*/
		vcharset(GRAPHICS);							/* Turn on graphics mode.		*/
		vprint("%c",text[i]);							/* Output the pointer character.	*/
		vcharset(DEFAULT);							/* Put back to normal.			*/
	}

	VL_vbuffering_end();								/* End of logical section.		*/

	return(SUCCESS);
}
/*
**	History:
**	$Log: vmenu.c,v $
**	Revision 1.22  2003/06/23 15:28:04  gsl
**	VL_ global symbols
**	
**	Revision 1.21  2003/02/04 18:43:33  gsl
**	fix -Wall warnings
**	
**	Revision 1.20  2003/01/31 20:58:40  gsl
**	Fix -Wall warnings
**	
**	Revision 1.19  2003/01/31 19:25:56  gsl
**	Fix copyright header
**	
**	Revision 1.18  2002/07/17 21:06:02  gsl
**	VL_ globals
**	
**	Revision 1.17  2002/07/16 13:40:22  gsl
**	VL_ globals
**	
**	Revision 1.16  2002/07/15 20:56:39  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.15  2002/07/15 20:16:10  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.14  2002/07/15 17:10:04  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.13  1997/07/08 21:18:16  gsl
**	change to use newe interface
**	
**	Revision 1.12  1996-10-11 18:16:11-04  gsl
**	drcs update
**
**
**
*/
