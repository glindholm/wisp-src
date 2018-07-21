/*
******************************************************************************
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
**
** NOTICE:
** Confidential, unpublished property of Shell Stream Software LLC.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of Shell Stream Software LLC
** is strictly prohibited.
** 
******************************************************************************
*/


/*
**	File:		menu.h
**
**	Project:	WISPLIB
**
**	Purpose:	menu system
**
*/

#ifndef menu_H
#define menu_H


						/* menu structures header file */

struct menuitem	{						/* The structure of an individual menu item		*/
			int    action;				/* Action code						*/
			char   title[81];			/* Title description of the item			*/
			char   filename[81];			/* The filename or command syntax for the item		*/
			char   message[81];			/* A message string to return to the menu caller	*/
		};

struct menu	{						/* The structure of a complete menu			*/
			int    curritem;			/* The Currently highlited item				*/
			int    menustat;			/* The status of this menu 0=new, 1=currently displayed	*/
			char   menutitle[81];			/* The title of the menu				*/
			struct menuitem menulist[30];		/* The list of menu items in the menu			*/
		};


									/* These are the action item codes			*/

#define	RUN_PROGRAM			1				/* Run a program					*/
#define EXECUTE_AND_PAUSE		2				/* Execute a command, wait for a keypress		*/
#define	EXECUTE_COMMAND			3				/* Execute a command					*/
#define DISPLAY_SUBTITLE		4				/* Display a subtitle					*/
#define	RETURN_A_VALUE			7				/* Only return a menu value				*/
#define DISPLAY_MENU			8				/* Display a new menu					*/
#define EXIT_PROGRAM			9				/* Exit the program					*/
#define LOGOFF				10				/* Log off the system					*/


struct menu *WL_menu_read(char* pmenufile);
int  WL_menu_go(char* menuname, char* menuvalue);
int  WL_menu_get_key(int key_num);
int  WL_menu_set_key(int key_num, int value);
int  WL_menu_scan(struct menu *themenu);
void WL_menu(char *name,	/* The name of the menu file			*/
	     char *rval);	/* The return value				*/

#endif /* menu_H */

/*
**	History:
**	$Log: menu.h,v $
**	Revision 1.11  2003/01/31 19:18:00  gsl
**	Fix copyright header  and -Wall warnings
**	
**	Revision 1.10  2002/07/10 21:05:19  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.9  1996/07/17 16:19:50  gsl
**	Fix missing newline
**	
**	Revision 1.8  1996-07-17 09:19:11-07  gsl
**	Add headers and prototypes for NT
**
**	
**
*/

