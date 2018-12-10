/*
** Copyright (c) Shell Stream Software LLC, All Rights Reserved.
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
*/


#include <stdio.h>
#include "idsistd.h"
#include "werrlog.h"
#include "menu.h"
#include "video.h"

static int menu_exit_key = 0;									/* Global var. Key 0 -  Exit key value. */

/* MENU_GET_KEY.C ... This routine accepts one parameters: 1 - Which global pfkey/action variable value to get; 		*/

int WL_menu_get_key(int key_num) 							/* Passed interger value.		*/
{
	if (!menu_exit_key) menu_exit_key = fn16_key;					/* Default to PF16.			*/
	switch (key_num)
	{
		case 0: return(menu_exit_key);						/* Return the key value.		*/
		default:
		{
			char	buff[80];
			sprintf(buff,"WL_menu_get_key() - Unsupported menu key number: %d", key_num);
			WL_werr_message_box(buff);
 			break;
		}
	}
	return 0;
}

/* MENU_SET_KEY.C ... This routine accepts two parameters: 1 - Which global pfkey/action variable to set; 2 - The value to be	*/
/* 		      recognized for the specified pfkey/action.								*/

int WL_menu_set_key(int key_num, int value)					/* Passed interger values.		*/
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
			sprintf(buff,"WL_menu_set_key() - Unsupported menu key number: %d", key_num);
			WL_werr_message_box(buff);
 			break;
		}
	}
	return 0;
}
/*
**	History:
**	$Log: menu_key.c,v $
**	Revision 1.14  2003/01/31 17:33:55  gsl
**	Fix  copyright header
**	
**	Revision 1.13  2002/07/10 21:05:20  gsl
**	Fix globals WL_ to make unique
**	
**	Revision 1.12  1997/05/21 15:43:43  gsl
**	Replace extern with include video.h
**	
**	Revision 1.11  1996-08-19 18:32:30-04  gsl
**	drcs update
**
**
**
*/
