			/************************************************************************/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*			Copyright (c) 1988, 1989, 1990			*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/************************************************************************/

#include "idsistd.h"
int menu_exit_key = 0;									/* Global var. Key 0 -  Exit key value. */

/* MENU_GET_KEY.C ... This routine accepts one parameters: 1 - Which global pfkey/action variable value to get; 		*/

menu_get_key(key_num) int key_num;							/* Passed interger value.		*/
{
	extern int fn16_key;								/* Reference VIDEO's fn16 key.		*/
	if (!menu_exit_key) menu_exit_key = fn16_key;					/* Default to PF16.			*/
	switch (key_num)
	{
		case 0: return(menu_exit_key);						/* Return the key value.		*/
		default:
		{
			vre("menu_get_key() - Unsupported menu key number: %d", key_num);
 			break;
		}
	}
}

/* MENU_SET_KEY.C ... This routine accepts two parameters: 1 - Which global pfkey/action variable to set; 2 - The value to be	*/
/* 		      recognized for the specified pfkey/action.								*/

menu_set_key(key_num, value) int key_num, value;					/* Passed interger values.		*/
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
			vre("menu_set_key() - Unsupported menu key number: %d", key_num);
 			break;
		}
	}
}
