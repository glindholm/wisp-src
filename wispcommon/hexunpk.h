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
**	File:		hexunpk.h
**
**	Project:	WISPLIB
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
**	Revision 1.2  2003/01/31 19:26:33  gsl
**	Fix copyright header
**	
**	Revision 1.1  1997/02/20 14:40:14  gsl
**	Initial revision
**	
**
**
**
*/
