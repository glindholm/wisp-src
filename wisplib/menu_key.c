static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*			Copyright (c) 1988, 1989, 1990			*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/

#include <stdio.h>
#include "idsistd.h"
#include "werrlog.h"
#include "menu.h"
#include "video.h"

int menu_exit_key = 0;									/* Global var. Key 0 -  Exit key value. */

/* MENU_GET_KEY.C ... This routine accepts one parameters: 1 - Which global pfkey/action variable value to get; 		*/

int menu_get_key(int key_num) 							/* Passed interger value.		*/
{
	if (!menu_exit_key) menu_exit_key = fn16_key;					/* Default to PF16.			*/
	switch (key_num)
	{
		case 0: return(menu_exit_key);						/* Return the key value.		*/
		default:
		{
			char	buff[80];
			sprintf(buff,"menu_get_key() - Unsupported menu key number: %d", key_num);
			werr_message_box(buff);
 			break;
		}
	}
	return 0;
}

/* MENU_SET_KEY.C ... This routine accepts two parameters: 1 - Which global pfkey/action variable to set; 2 - The value to be	*/
/* 		      recognized for the specified pfkey/action.								*/

int menu_set_key(int key_num, int value)					/* Passed interger values.		*/
{
	switch (key_num)
	{
		case 0:
		{
			menu_exit_key = value;
			break;
		}
		default:
		{
			char	buff[80];
			sprintf(buff,"menu_set_key() - Unsupported menu key number: %d", key_num);
			werr_message_box(buff);
 			break;
		}
	}
	return 0;
}
/*
**	History:
**	$Log: menu_key.c,v $
**	Revision 1.12  1997/05/21 15:43:43  gsl
**	Replace extern with include video.h
**	
**	Revision 1.11  1996-08-19 18:32:30-04  gsl
**	drcs update
**
**
**
*/
