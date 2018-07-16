			/************************************************************************/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		 Copyright (c) 1988, 1989, 1990, 1991, 1992		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/

#include <stdio.h>
#ifdef unix
#include <sys/types.h>
#endif
#include <time.h>
#include "menu.h"
#include "wperson.h"
#include <v/video.h>
#include <v/vlocal.h>
#include <v/vdata.h>

/* Construct a Menu stored in the structure "themenu" on the screen and let the user select an item, return the item to caller	*/

int menu_scan(themenu) struct menu *themenu;
{
	int i,tx,ty;
	char username[34];
	int key, meta_exit_key, exit_key;


	if (wbackground()) return(-1);							/* Is in background so return.		*/

	meta_exit_key = menu_get_key(0);						/* Get the metacharacter exit key.	*/
	exit_key = vfnkey(meta_exit_key);						/* Get the exit code.			*/

	if (themenu->menustat == 1) goto get_input;					/* Just scan if already on, 		*/
											/* Otherwise draw the menu...		*/
	vmove(0,40-(strlen(themenu->menutitle) / 2));					/* Start with centered title.		*/
	vprint("%s",themenu->menutitle);
	vmove(2,0);
	cuserid(username);								/* Get the user's ID. 			*/
	vprint(" Username : %s",username);						/* Put it up.				*/
	vcurrtime(2,35);								/* Also the time.			*/
	vmove(3,0);
/*	vline(HORIZONTAL,79);	*/							/* Draw a line under the title		*/
		   /*12345678901234567890123456789012345678901234567890123456789012345678901234567890*/
	vprint("%s"," ------------------------------------------------------------------------------");
	vmove(22,1);
	vprint("(%d) Return to Program, (TAB) (SPACE) or (ARROWS) to move. (RETURN) to Select", exit_key);

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

	vmove(tx,ty);										/* Move cursor to current loc	*/

	for (;;)
	{
		vcurrtime(2,35);								/* Report the current time.	*/
		key = vgetm();									/* Get a meta character.	*/
		vcurrtime(2,35);								/* Report current time.		*/

		if (key == meta_exit_key)							/* Did they hit the exit ?	*/
		{
			vdefer(RESTORE);
			vstate(DEFAULT);							/* Set terminal to normal.	*/
			return(-1);								/* Return (maybe exit)?		*/
			break;
		}

		else if (key == home_key) home_item(themenu);					/* Home?			*/

		else if (key == up_arrow_key) up_one_item(themenu);				/* Up?				*/

		else if (key == down_arrow_key) down_one_item(themenu);				/* Down?			*/

		else if (key == left_arrow_key) left_one_item(themenu);				/* Left?			*/

		else if (key == right_arrow_key) right_one_item(themenu);			/* Right?			*/

		else if ((key == enter_key) || (key == return_key))				/* Enter?			*/
		{
			return(themenu->curritem);
			break;
		}

		else if (key == help_key) ws_help(OFF);						/* Help?			*/

		else if (key == ' ') down_one_item(themenu);					/* Space bar?			*/

		else if (key == backtab_key) up_one_item(themenu);				/* Backspace? 			*/

		else if (key == tab_key) right_one_item(themenu);				/* Horizontal Tab?		*/

		else if (key == delete_key) left_one_item(themenu);				/* Delete?			*/

		else if ((key >= 'A') && (key >= 'Z')) find_item(key,themenu);			/* Everything else.		*/
		else if ((key >= 'a') && (key >= 'z')) find_item(key,themenu);			/* Everything else.		*/
		else if ((key >= '0') && (key >= '9')) find_item(key,themenu);			/* Everything else.		*/

		else vbell();									/* Unknown so ring a bell.	*/

	}
}


up_one_item(themenu) struct menu *themenu;
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
			vmove(i < 15 ? 5+i : i-10 ,i < 15 ? 4 : 42);
			return(0);
		}
	}
}

down_one_item(themenu)
struct menu *themenu;
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
			vmove(i < 15 ? 5+i : i-10 ,i < 15 ? 4 : 42);
			return(0);
		}
	} 											/* end of for */
}


left_one_item(themenu)
struct menu *themenu;
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
			vmove(i < 15 ? 5+i : i-10 ,i < 15 ? 4 : 42);
			return(0);
		}
		else i = i < 15 ? i + 15 : i - 14;
	} 											/* end of for */
}

right_one_item(themenu)
struct menu *themenu;
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
			vmove(i < 15 ? 5+i : i-10 ,i < 15 ? 4 : 42);
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


home_item(themenu)
struct menu *themenu;
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
			vmove(i < 15 ? 5+i : i-10 ,i < 15 ? 4 : 42);
			return(0);
		}
	} 											/* end of for */
}



find_item(thekey,themenu) int thekey; struct menu *themenu;
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
			vmove(i < 15 ? 5+i : i-10 ,i < 15 ? 4 : 42);
			return(0);
		}
		else
		{
			if (i == themenu->curritem)
			{
				vmove(i < 15 ? 5+i : i-10 ,i < 15 ? 4 : 42);
				return(0);
			}
		}
	} 											/* end of for 			*/
}

draw_item(row,col,mode,pref,item)
int row,col,mode;
char *pref,*item;									/* Draw the item on the screen		*/
{
	vmove(row,col);
	vmode(mode);

	if (mode == UNDERSCORE) 							/* Underscore means no prefix code	*/
	{
		vmove(row,col+2);							/* move over 2 spaces			*/
		vprint("%s",item);
	}
	else
	{
		vprint("%s%s",pref,item);
	}

	vmode(CLEAR);
	return(0);
}


vcurrtime(irow,icol)

int irow,icol;

{
	struct tm *time_structure;
	time_t time_val;
	long i;
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

	sprintf(timestring,"%s, %s %d, 19%d    %d:%02d %s",weekday[time_structure->tm_wday],month[time_structure->tm_mon],
			time_structure->tm_mday,time_structure->tm_year,time_structure->tm_hour,time_structure->tm_min,hour[i]);
	vmove(irow,icol);
	vprint("%s",timestring);

	return(0);
}
