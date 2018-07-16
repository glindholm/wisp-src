			/************************************************************************/
			/*									*/
			/*	     VIDEO - Video Interactive Development Environment		*/
			/*									*/
			/*			    Copyright (c) 1987				*/
			/*									*/
			/*	An unpublished work by Greg L. Adams.  All rights reserved.	*/
			/*									*/
			/************************************************************************/


/*						Include standard header files.							*/

#include "video.h"									/* Include video definitions.		*/
#include "vlocal.h"									/* Include local definitions.		*/


/*						Subroutine entry point.								*/

vmap(action,sl,sc,el,ec) int action,sl,sc,el,ec;					/* Perform action on screen map.	*/
{
	extern int optimization;							/* Master optimization control.		*/
	register int i, j;								/* Working registers.			*/

	if ((action == CLEAR) || (action == TAG_AS_OLD))
	{
		if (optimization == 0) return(FAILURE);					/* Cannot do if not tracking.		*/

		if ((action == CLEAR) && (sl+sc == 0) && (el == MAX_LINES_PER_SCREEN-1) && (ec == MAX_COLUMNS_PER_LINE-1))
		{
			vmp_express();							/* Express clear the screen maps.	*/
		}
		else if (sl == el)							/* Action is only on one line.		*/
		{
			for (j = sc; j <= ec; j++) vmp_x(action,sl,j);			/* Do the requested action.		*/
		}
		else									/* Action is over multiple lines.	*/
		{
			for (i = sl; i <= el; i++)					/* Loop through all lines.		*/
			{
				if (i == sl)						/* Is this the first line.		*/
				{
					for (j = sc; j < MAX_COLUMNS_PER_LINE; j++) vmp_x(action,i,j);
				}
				else if (i == el)					/* Is this the last line.		*/
				{
					for (j = 0; j <= ec; j++) vmp_x(action,i,j);
				}
				else							/* No, then do the full line.		*/
				{
					for (j = 0; j < MAX_COLUMNS_PER_LINE; j++) vmp_x(action,i,j);
				}
			}
		}
	}

	else if (action == SCROLL_UP) vmp_up(sl,el);					/* Scroll the map up.			*/

	else if (action == SCROLL_DOWN) vmp_down(sl,el);				/* Scroll the map down.			*/

	else
	{
		vre("vmap(%d)-Invalid parameter value",action);				/* Report the occurance.		*/
		return(FAILURE);							/* Oops, invalid parameter.		*/
	}

	return(SUCCESS);								/* Return to the caller.		*/
}


/*				Do the requested action on a particular cell.							*/

vmp_x(action,i,j) int action,i,j;							/* Perform the requested action.	*/
{
	extern char vchr_map[MAX_LINES_PER_SCREEN][MAX_COLUMNS_PER_LINE];		/* Reference the character map.		*/
	extern char vatr_map[MAX_LINES_PER_SCREEN][MAX_COLUMNS_PER_LINE];		/* Reference the attribute map.		*/
	extern char vmap_cng[MAX_LINES_PER_SCREEN][MAX_COLUMNS_PER_LINE];		/* Reference the change map.		*/
	register int k;									/* Working registers.			*/

	k = vml(i);									/* Get the table index.			*/
	if (action == CLEAR)
	{
		vchr_map[k][j] = ' ';							/* Set the character map to space.	*/
		vatr_map[k][j] = 0;							/* Set the attribute map to null.	*/
		vmap_cng[k][j] = 0;							/* And the screen matches the map.	*/
	}
	if (action == TAG_AS_OLD)
	{
		if (!visible(vchr_map[k][j],vatr_map[k][j])) vmap_cng[k][j] = 0;	/* If not visible, all is ok...		*/
		else vmap_cng[k][j] = -1;						/*    else flag as old data.		*/
	}
	vdc(i);										/* Decrement the global counts.		*/
	return(SUCCESS);								/* Return to the caller.		*/
}


/*					Clear global counts on a line.								*/

vmp_clr(line) int line;									/* Clear global counts on a line.	*/
{
	extern int vscr_cng, vlin_cng[MAX_LINES_PER_SCREEN];				/* Reference global count data.		*/

	vlin_cng[line] = 0;								/* And now no changes on this line.	*/
	return(SUCCESS);								/* Return to the caller.		*/
}

/*					Scroll the map up.									*/

vmp_up(sl,el) int sl,el;								/* From start line to end line.		*/
{
	extern int vcur_col, vcur_lin, vrol_bot, vscr_wid, vmap_top;			/* Reference VIDEO data base.		*/
	extern char vchr_map[MAX_LINES_PER_SCREEN][MAX_COLUMNS_PER_LINE];		/* Reference character map.		*/
	extern char vatr_map[MAX_LINES_PER_SCREEN][MAX_COLUMNS_PER_LINE];		/* Reference attribute map.		*/
	extern char vmap_cng[MAX_LINES_PER_SCREEN][MAX_COLUMNS_PER_LINE];		/* Reference change map.		*/
	extern int vlin_cng[MAX_LINES_PER_SCREEN];					/* Reference line change table.		*/
	extern int vlin_atr[MAX_LINES_PER_SCREEN];					/* Reference line attribute table.	*/
	register int i,j,k0,k1;								/* Working registers.			*/

	if ((sl == 0) && (el == MAX_LINES_PER_SCREEN-1))				/* Is this a full screen scroll?	*/
	{
		vmp_clr(0);								/* Clear global counts.			*/
		memset(&vchr_map[vmap_top][0],' ',vscr_wid);				/* Fill with spaces.			*/
		memset(&vatr_map[vmap_top][0],'\0',vscr_wid);				/* No attributes set on the new line.	*/
		memset(&vmap_cng[vmap_top][0],'\0',vscr_wid);				/* No changes on this character.	*/
		vmap_top = vmap_top + 1;						/* Adjust the virtual map top.		*/
		if (vmap_top == MAX_LINES_PER_SCREEN) vmap_top = 0;			/* Wrap around if necessary.		*/
	}

	else										/* Not full screen, do the hard way.	*/
	{
		k0 = vml(sl);								/* get initial value			*/
		for (i = sl+1; i <= el; i++)						/* Loop through each line.		*/
		{
			k1 = vml(i);							/* Get index into the table.		*/

			memcpy(&vchr_map[k0][0],&vchr_map[k1][0],vscr_wid);		/* Shuffle the bytes.			*/
			memcpy(&vatr_map[k0][0],&vatr_map[k1][0],vscr_wid);
			memcpy(&vmap_cng[k0][0],&vmap_cng[k1][0],vscr_wid);

			k0 = k1;
		}
											/* Now blank the bottom line.		*/
		memset(&vchr_map[k0][0],' ',vscr_wid);					/* Fill with spaces.			*/
		memset(&vatr_map[k0][0],'\0',vscr_wid);					/* No attributes set on the new line.	*/
		memset(&vmap_cng[k0][0],'\0',vscr_wid);					/* No changes on this character.	*/
		vmp_clr(i);								/* Clear global counts.			*/
	}

	for (j = sl; j < el; j++)							/* Now copy the line attributes.	*/
	{
		vlin_atr[j] = vlin_atr[j+1];						/* Scroll attribute table.		*/
		vlin_cng[j] = vlin_cng[j+1];						/* Scroll the change table.		*/
	}

	vlin_atr[el] = SINGLE_WIDTH;							/* Now the line is single width again.	*/
	vlin_cng[el] = 0;								/* And the line is now unchanged.	*/

	return(SUCCESS);								/* What could possibly go wrong?	*/
}

/*					Scroll the map down.									*/

vmp_down(sl,el) int sl,el;								/* From start line to end line.		*/
{
	extern int vcur_col, vcur_lin, vrol_bot, vscr_wid, vmap_top;			/* Reference VIDEO data base.		*/
	extern char vchr_map[MAX_LINES_PER_SCREEN][MAX_COLUMNS_PER_LINE];		/* Reference character map.		*/
	extern char vatr_map[MAX_LINES_PER_SCREEN][MAX_COLUMNS_PER_LINE];		/* Reference attribute map.		*/
	extern char vmap_cng[MAX_LINES_PER_SCREEN][MAX_COLUMNS_PER_LINE];		/* Reference change map.		*/
	extern int vlin_cng[MAX_LINES_PER_SCREEN];					/* Reference line change table.		*/
	extern int vlin_atr[MAX_LINES_PER_SCREEN];					/* Reference line attribute table.	*/
	register int i,j,k0,k1;								/* Working registers.			*/

	if ((sl == 0) && (el == MAX_LINES_PER_SCREEN-1))				/* Is this a full screen scroll?	*/
	{
		vmp_clr(23);								/* Clear global counts.			*/
		vmap_top = vmap_top - 1;						/* Adjust the virtual map top.		*/
		if (vmap_top < 0) vmap_top = MAX_LINES_PER_SCREEN - 1;			/* Wrap around if necessary.		*/
		memset(&vchr_map[vmap_top][0],' ',vscr_wid);				/* Fill with spaces.			*/
		memset(&vatr_map[vmap_top][0],'\0',vscr_wid);				/* No attributes set on the new line.	*/
		memset(&vmap_cng[vmap_top][0],'\0',vscr_wid);				/* No changes on this character.	*/
	}

	else										/* Not full screen, do the hard way.	*/
	{
		k0 = vml(el);								/* get initial value			*/
		for (i = el-1; i >= sl; i--)						/* Loop through each line.		*/
		{
			k1 = vml(i);							/* Get index into the table.		*/

			memcpy(&vchr_map[k0][0],&vchr_map[k1][0],vscr_wid);		/* Shuffle the bytes.			*/
			memcpy(&vatr_map[k0][0],&vatr_map[k1][0],vscr_wid);
			memcpy(&vmap_cng[k0][0],&vmap_cng[k1][0],vscr_wid);

			k0 = k1;
		}
											/* Now blank the bottom line.		*/
		vmp_clr(i);								/* Clear global counts.			*/
		memset(&vchr_map[k0][0],' ',vscr_wid);					/* Fill with spaces.			*/
		memset(&vatr_map[k0][0],'\0',vscr_wid);					/* No attributes set on the new line.	*/
		memset(&vmap_cng[k0][0],'\0',vscr_wid);					/* No changes on this character.	*/
	}

	for (j = el; j > el; j--)							/* Now copy the line attributes.	*/
	{
		vlin_atr[j] = vlin_atr[j-1];						/* Scroll attribute table.		*/
		vlin_cng[j] = vlin_cng[j-1];						/* Scroll the change table.		*/
	}

	vlin_atr[sl] = SINGLE_WIDTH;							/* Now the line is single width again.	*/
	vlin_cng[sl] = 0;								/* And the line is now unchanged.	*/

	return(SUCCESS);								/* What could possibly go wrong?	*/
}
vmp_express()
{
	extern char vchr_map[MAX_LINES_PER_SCREEN][MAX_COLUMNS_PER_LINE];		/* Reference the character map.		*/
	extern char vatr_map[MAX_LINES_PER_SCREEN][MAX_COLUMNS_PER_LINE];		/* Reference the attribute map.		*/
	extern char vmap_cng[MAX_LINES_PER_SCREEN][MAX_COLUMNS_PER_LINE];		/* Reference the change map.		*/
	extern int vscr_cng, vlin_cng[MAX_LINES_PER_SCREEN];				/* Reference the global change counts.	*/
	register int i;									/* Working register.			*/

	memset(vchr_map, ' ',MAX_LINES_PER_SCREEN*MAX_COLUMNS_PER_LINE);		/* Clear the maps...			*/
	memset(vatr_map,'\0',MAX_LINES_PER_SCREEN*MAX_COLUMNS_PER_LINE);
	memset(vmap_cng,'\0',MAX_LINES_PER_SCREEN*MAX_COLUMNS_PER_LINE);
	imemset(vlin_cng, 0 ,MAX_LINES_PER_SCREEN);
	vscr_cng = 0;									/* Set the global counts to 0;		*/
	return(SUCCESS);								/* Return to the caller.		*/
}
