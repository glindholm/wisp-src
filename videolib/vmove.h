/* 
	Copyright (c) 1996 DevTech Migrations, All rights reserved.
	$Id:$
*/

/*
**	File:		vmove.h
**
**	Project:	???
**
**	RCS:		$Source:$
**
**	Purpose:	???
**
*/

#ifndef vmove_H
#define vmove_H
/*
**	Includes
*/

/*
**	Structures and Defines
*/

/*
**	Function Prototypes
*/
int vmove(int line, int column);							/* Move to a location on the screen.	*/
int vgoto(int nl, int nc, int ol, int oc, int op);					/* Got to (nl,nc) from (ol,oc).		*/

#endif /* vmove_H */

/*
**	History:
**	$Log: vmove.h,v $
**	Revision 1.2  1997-07-09 11:06:29-04  gsl
**	Remove static routines
**
**	Revision 1.1  1996-03-28 17:23:41-05  gsl
**	Initial revision
**
**
*/
