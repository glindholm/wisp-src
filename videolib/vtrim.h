/* 
	Copyright (c) 1996 DevTech Migrations, All rights reserved.
	$Id:$
*/

/*
**	File:		vtrim.h
**
**	Project:	???
**
**	RCS:		$Source:$
**
**	Purpose:	???
**
*/

#ifndef vtrim_H
#define vtrim_H
/*
**	Includes
*/

/*
**	Structures and Defines
*/

/*
**	Function Prototypes
*/
int vtrim(char *string);								/* Trim a string of trailing blanks.	*/
int vtrimlen(char *outstr, char *instr, int length);					/* Trim a string of length.		*/
int vputlen(char *outstr, char *instr, int length);					/* Opposite of vtrimlen.		*/

#endif /* vtrim_H */

/*
**	History:
**	$Log: vtrim.h,v $
**	Revision 1.1  1996-03-28 16:42:52-05  gsl
**	Initial revision
**
**
*/
