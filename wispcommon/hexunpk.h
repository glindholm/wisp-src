/* 
	Copyright (c) 1997 NeoMedia Migrations, All rights reserved.
	$Id:$
*/

/*
**	File:		hexunpk.h
**
**	Project:	WISPLIB
**
**	RCS:		$Source:$
**
**	Purpose:	Header for HEXPACK and HEXUNPK
**
*/

#ifndef hexunpk_H
#define hexunpk_H
/*
**	Includes
*/

/*
**	Structures and Defines
*/

/*
**	Function Prototypes
*/
void HEXUNPK( char* source,  char* target, int4 *tlen );
void HEXPACK( char* source,  char* target, int4* tlen );

#endif /* hexunpk_H */

/*
**	History:
**	$Log: hexunpk.h,v $
**	Revision 1.1  1997/02/20 14:40:14  gsl
**	Initial revision
**	
**
**
**
*/
