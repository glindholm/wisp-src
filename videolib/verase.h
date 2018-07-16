/* 
	Copyright (c) 1996 DevTech Migrations, All rights reserved.
	$Id:$
*/

/*
**	File:		verase.h
**
**	Project:	???
**
**	RCS:		$Source:$
**
**	Purpose:	???
**
*/

#ifndef verase_H
#define verase_H
/*
**	Includes
*/
#include "vmenu.h"

/*
**	Structures and Defines
*/

/*
**	Function Prototypes
*/
int verase(int control);								/* Erase all or part of the screen.	*/
int verase_menu(int control, struct video_menu *md);					/* Erase all, top or bottom menus.	*/

#endif /* verase_H */

/*
**	History:
**	$Log: verase.h,v $
**	Revision 1.1  1996-03-28 16:32:35-05  gsl
**	Initial revision
**
**
*/
