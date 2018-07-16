/* 
	Copyright (c) 1996 DevTech Migrations, All rights reserved.
	$Id:$
*/

/*
**	File:		vline.h
**
**	Project:	VIDEO/LIB
**
**	RCS:		$Source:$
**
**	Purpose:	???
**
*/

#ifndef vline_H
#define vline_H
/*
**	Includes
*/

/*
**	Structures and Defines
*/

/*
**	Function Prototypes
*/
#ifdef vline
#undef vline
#endif
int vline(int type, int length);							/* Draw a line.				*/

#endif /* vline_H */

/*
**	History:
**	$Log: vline.h,v $
**	Revision 1.1  1996-03-28 16:20:26-05  gsl
**	Initial revision
**
**
*/
