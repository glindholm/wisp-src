/* 
	Copyright (c) 1996 DevTech Migrations, All rights reserved.
	$Id:$
*/

/*
**	File:		platsubs.h
**
**	Project:	???
**
**	RCS:		$Source:$
**
**	Purpose:	???
**
*/

#ifndef platsubs_H
#define platsubs_H
/*
**	Includes
*/

/*
**	Structures and Defines
*/

/*
**	Function Prototypes
*/
int 	valplat(char code[2]);
int 	whatplat(char* name, char code[2]);
int	plat_num(int num, char* name, char code[2]);
int	plat_code(char code[2], char* name, int* num);
void 	putplattab(void);





#endif /* platsubs_H */

/*
**	History:
**	$Log: platsubs.h,v $
**	Revision 1.2  1996/07/24 22:57:53  gsl
**	Move from wisp/lib to wisp/common
**	
**	Revision 1.1  1996-06-28 14:45:26-07  gsl
**	Initial revision
**
**
**
*/
