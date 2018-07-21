/*
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
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
*/


#include <stdio.h>
#include <sys/types.h>
#include <time.h>
#include <string.h>
#include <ctype.h>

#include "idsistd.h"
#include "menu.h"
#include "wperson.h"
#include "wisplib.h"
#include "vwang.h"
#include "wanguid.h"

#include "video.h"
#include "vlocal.h"
#include "vdata.h"

static int up_one_item(struct menu *themenu);
static int down_one_item(struct menu *themenu);
static int left_one_item(struct menu *themenu);
static int right_one_item(struct menu *themenu);
static int home_item(struct menu *themenu);
static int find_item(int thekey, struct menu *themenu);
static int draw_item(int row, int col,int mode,char* pref,char* item);
static int vcurrtime(int irow, int icol);

/* Construct a Menu stored in the structure "themenu" on the screen and let the user select an item, return the item to caller	*/

int WL_menu_scan(struct menu *themenu)
{
	int i,tx,ty;
	char username[34];
	int key, meta_exit_key, exit_key;


	if (wbackground()) return(-1);							/* Is in background so return.		*/

	meta_exit_key = WL_menu_get_key(0);						/* Get the metacharacter exit key.	*/
	exit_key = VL_vfnkey(meta_exit_key);						/* Get the exit code.			*/

	if (themenu->menustat == 1) goto get_input;					/* Just scan if already on, 		*/
											/* Otherwise draw the menu...		*/
	VL_vmove(0,40-(strlen(themenu->menutitle) / 2));					/* Start with centered title.		*/
	VL_vprint("%s",themenu->menutitle);
	VL_vmove(2,0);
	strcpy(username, WL_longuid());							/* Get the user's ID. 			*/
	VL_vprint(" Username : %s",username);						/* Put it up.				*/
	vcurrtime(2,35);								/* Also the time.			*/
	VL_vmove(3,0);
/*	VL_vline(HORIZONTAL,79);	*/							/* Draw a line under the title		*/
		   /*12345678901234567890123456789012345678901234567890123456789012345678901234567890*/
	VL_vprint("%s"," ------------------------------------------------------------------------------");
	VL_vmove(22,1);
	VL_vprint("(%d) Return to Program, (TAB) (SPACE) or (ARROWS) to move. (RETURN) to Select", exit_key);

	for (i=0; i<30;i++)								/* output each menu Item		*/
	{
		if (themenu->menulist[i].action)
		{
											/* Move to first char pos.	 	*/
			if (i == themenu->curritem)					/* Highlite current active item		*/
			{
				draw_item(i < 15 ? 5+i : i-10 ,i < 15 ? 4 : 42,	BOLD,"* ", &(themenu->menulist[i].title[0]));
			}
			else if (themenu->menulist[i].action == DISPLAY_SUBTITLE)	/* Display a subtitle item		*/
			{
				draw_item(i < 15 ? 5+i : i-10 ,i < 15 ? 4 : 42, UNDERSCORE,"", &(themenu->menulist[i].title[0]));
			}
			else
			{								/* Draw a normal item			*/
				draw_item(i < 15 ? 5+i : i-10 ,i < 15 ? 4 : 42, CLEAR,"_ ", &(themenu->menulist[i].title[0]));
			}
		}
	}


get_input:

	i = themenu->curritem;
	tx = i < 15 ? 5+i : i-10;
	ty = i < 15 ? 4 : 42;

	VL_vmove(tx,ty);										/* Move cursor to current loc	*/

	for (;;)
	{
		vcurrtime(2,35);								/* Report the current time.	*/
		key = VL_vgetm();									/* Get a meta character.	*/
		vcurrtime(2,35);								/* Report current time.		*/

		if (key == meta_exit_key)							/* Did they hit the exit ?	*/
		{
			VL_vdefer_restore();
			VL_vstate(VSTATE_DEFAULT);							/* Set terminal to normal.	*/
			return(-1);								/* Return (maybe exit)?		*/
		}

		else if (key == home_key) home_item(themenu);					/* Home?			*/

		else if (key == up_arrow_key) up_one_item(themenu);				/* Up?				*/

		else if (key == down_arrow_key) down_one_item(themenu);				/* Down?			*/

		else if (key == left_arrow_key) left_one_item(themenu);				/* Left?			*/

		else if (key == right_arrow_key) right_one_item(themenu);			/* Right?			*/

		else if ((key == enter_key) || (key == return_key))				/* Enter?			*/
		{
			return(themenu->curritem);
		}

		else if (key == help_key) vwang_help(OFF);						/* Help?			*/

		else if (key == ' ') down_one_item(themenu);					/* Space bar?			*/

		else if (key == backtab_key) up_one_item(themenu);				/* Backspace? 			*/

		else if (key == tab_key) right_one_item(themenu);				/* Horizontal Tab?		*/

		else if (key == delete_key) left_one_item(themenu);				/* Delete?			*/

		else if ((key >= 'A') && (key >= 'Z')) find_item(key,themenu);			/* Everything else.		*/
		else if ((key >= 'a') && (key >= 'z')) find_item(key,themenu);			/* Everything else.		*/
		else if ((key >= '0') && (key >= '9')) find_item(key,themenu);			/* Everything else.		*/

		else VL_vbell();									/* Unknown so ring a bell.	*/

	}
}


static int up_one_item(struct menu *themenu)
{
	int i;
	i = themenu->curritem;									/* un highlite the current item	*/
	draw_item(i < 15 ? 5+i : i-10 ,i < 15 ? 4 : 42, CLEAR,"_ ", &(themenu->menulist[i].title[0]));

	for (;;)										/* Search backward thru list.	*/
	{
		i--;
		if (i < 0)									/* Ran out of items.		*/
		i = 29;										/* Found an active item.	*/
		if ((themenu->menulist[i].action) && (themenu->menulist[i].action != DISPLAY_SUBTITLE))
		{
			themenu->curritem = i;							/* make this the current item	*/
			draw_item(i < 15 ? 5+i : i-10 ,i < 15 ? 4 : 42, BOLD,"* ", &(themenu->menulist[i].title[0]));
			VL_vmove(i < 15 ? 5+i : i-10 ,i < 15 ? 4 : 42);
			return(0);
		}
	}
}

static int down_one_item(struct menu *themenu)
{
	int i;
	i = themenu->curritem;								/* Unhilite the current item		*/
	draw_item(i < 15 ? 5+i : i-10 ,i < 15 ? 4 : 42,CLEAR,"_ ",&(themenu->menulist[i].title[0]));


	for (;;)							/* search backward through the item list till we find one */
	{
		i++;
		if (i >29) i = 0;							/* ran out of items going forwards */
		if ((themenu->menulist[i].action) && (themenu->menulist[i].action != DISPLAY_SUBTITLE))	/* found an active item */
		{
			themenu->curritem = i;
			draw_item(i < 15 ? 5+i : i-10 ,i < 15 ? 4 : 42,BOLD,"* ",&(themenu->menulist[i].title[0]));
			VL_vmove(i < 15 ? 5+i : i-10 ,i < 15 ? 4 : 42);
			return(0);
		}
	} 											/* end of for */
}


static int left_one_item(struct menu *themenu)
{
	int i;
	i = themenu->curritem;
	draw_item(i < 15 ? 5+i : i-10 ,i < 15 ? 4 : 42, CLEAR,"_ ",&(themenu->menulist[i].title[0]));
	if ( i == 0)
	{
		i = 30;
	}
	else
	{
		i = i < 15 ? i + 15 : i - 14;
 	}

	for (;;)							/* Search backward through item list till we find one.	*/
	{
		i--;
		if (i < 0) i = 29;
		if ((themenu->menulist[i].action) && (themenu->menulist[i].action != DISPLAY_SUBTITLE))	/* found an active item */
		{
			themenu->curritem = i;
			draw_item(i < 15 ? 5+i : i-10 ,i < 15 ? 4 : 42,BOLD,"* ",&(themenu->menulist[i].title[0]));
			VL_vmove(i < 15 ? 5+i : i-10 ,i < 15 ? 4 : 42);
			return(0);
		}
		else i = i < 15 ? i + 15 : i - 14;
	} 											/* end of for */
}

static int right_one_item(struct menu *themenu)
{
	int i;
	i = themenu->curritem;
	draw_item(i < 15 ? 5+i : i-10 ,i < 15 ? 4 : 42,CLEAR,"_ ",&(themenu->menulist[i].title[0]));
	if ( i == 29 )
	{
		i = -1;
	}
	else
	{
		i = i < 15 ? i + 14 : i - 15;
	}
	for (;;)							/* Search backward through item list till we find one.	*/
	{
		i++;
		if (i >29) i = 0;
		if ((themenu->menulist[i].action) && (themenu->menulist[i].action != DISPLAY_SUBTITLE))	/* found an active item */
		{
			themenu->curritem = i;
			draw_item(i < 15 ? 5+i : i-10 ,i < 15 ? 4 : 42, BOLD,"* ",&(themenu->menulist[i].title[0]));
			VL_vmove(i < 15 ? 5+i : i-10 ,i < 15 ? 4 : 42);
			return(0);
		}
		else 
		{
			if (i == 29)
			{
				i = -1;
			}
			else
			{
				i = i < 15 ? i + 14 : i - 15;
			}
		}
	} /* end of for */
}


static int home_item(struct menu *themenu)
{
	int i;
	i = themenu->curritem;
	draw_item(i < 15 ? 5+i : i-10 ,i < 15 ? 4 : 42,CLEAR,"_ ",&(themenu->menulist[i].title[0]));

	i = -1;

	for (;;)							/* Search forward through item list till we find one.	*/
	{
		i++;
		if (i >29) i = 0;
		if ((themenu->menulist[i].action) && (themenu->menulist[i].action != DISPLAY_SUBTITLE))	/* found an active item */
		{
			themenu->curritem = i;
			draw_item(i < 15 ? 5+i : i-10 ,i < 15 ? 4 : 42,BOLD,"* ",&(themenu->menulist[i].title[0]));
			VL_vmove(i < 15 ? 5+i : i-10 ,i < 15 ? 4 : 42);
			return(0);
		}
	} 											/* end of for */
}



static int find_item(int thekey, struct menu *themenu)
{
	int i;
	char keyval;
	keyval = toupper((char)thekey);

	i = themenu->curritem;

	for (;;)							/* Search forward through item list till we find one.	*/
	{
		i++;
		if (i >29) i = 0;
		if ( (themenu->menulist[i].action) &&
		     (toupper(themenu->menulist[i].title[0]) == keyval) && 
		     (themenu->menulist[i].action != DISPLAY_SUBTITLE)     )
		{
			draw_item(themenu->curritem < 15 ? 5+themenu->curritem : themenu->curritem-10 ,
			themenu->curritem < 15 ? 4 : 42,CLEAR,"_ ",&(themenu->menulist[themenu->curritem].title[0]));

			themenu->curritem = i;

			draw_item(i < 15 ? 5+i : i-10 ,i < 15 ? 4 : 42,BOLD,"* ",&(themenu->menulist[i].title[0]));
			VL_vmove(i < 15 ? 5+i : i-10 ,i < 15 ? 4 : 42);
			return(0);
		}
		else
		{
			if (i == themenu->curritem)
			{
				VL_vmove(i < 15 ? 5+i : i-10 ,i < 15 ? 4 : 42);
				return(0);
			}
		}
	} 											/* end of for 			*/
}

static int draw_item(int row, int col,int mode,char* pref,char* item)
{
	VL_vmove(row,col);
	VL_vmode(mode);

	if (mode == UNDERSCORE) 							/* Underscore means no prefix code	*/
	{
		VL_vmove(row,col+2);							/* move over 2 spaces			*/
		VL_vprint("%s",item);
	}
	else
	{
		VL_vprint("%s%s",pref,item);
	}

	VL_vmode(CLEAR);
	return(0);
}


static int vcurrtime(int irow, int icol)
{
	struct tm *time_structure;
	time_t time_val;
	int4 i;
	char timestring[80];

	static char *weekday[7] = {"Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"};

	static char *month[12] = {"January","February","March","April","May","June","July","August","September","October",
					"November","December"};

	static char *hour[2] = {"AM","PM"};

	time(&time_val);
	time_structure = localtime(&time_val);
	if (time_structure->tm_hour > 12)
	{
		time_structure->tm_hour = (time_structure->tm_hour) - 12;
		i = 1;
	}
	else i = 0;

	sprintf(timestring,"%s, %s %d, %d    %d:%02d %s",
		weekday[time_structure->tm_wday],
		month[time_structure->tm_mon],
		time_structure->tm_mday,
		time_structure->tm_year+1900,
		time_structure->tm_hour,
		time_structure->tm_min,
		hour[i]);

	VL_vmove(irow,icol);
	VL_vprint("%s",timestring);

	return(0);
}
/*
**	History:
**	$Log: menuscan.c,v $
**	Revision 1.20  2003/01/31 18:25:18  gsl
**	Fix  copyright header and -Wall warnings
**	
**	Revision 1.19  2003/01/31 17:33:55  gsl
**	Fix  copyright header
**	
**	Revision 1.18  2002/07/15 20:16:01  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.17  2002/07/15 17:52:50  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.16  2002/07/11 20:29:10  gsl
**	Fix WL_ globals
**	
**	Revision 1.15  2002/07/10 21:05:20  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.14  2001/11/27 22:07:54  gsl
**	Change to use longuid()
**	
**	Revision 1.13  1997-10-03 13:36:07-04  gsl
**	YEAR2000 fix
**
**	Revision 1.12  1997-07-09 12:41:12-04  gsl
**	Change to use new interface
**
**	Revision 1.11  1996-08-19 18:32:30-04  gsl
**	drcs update
**
**
**
*/
