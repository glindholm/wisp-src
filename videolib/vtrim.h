/*
******************************************************************************
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** $Id:$
**
** NOTICE:
** Confidential, unpublished property of NeoMedia Technologies, Inc.
** Use and distribution limited solely to authorized personnel.
** 
** The use, disclosure, reproduction, modification, transfer, or
** transmittal of this work for any purpose in any form or by
** any means without the written permission of NeoMedia 
** Technologies, Inc. is strictly prohibited.
** 
** CVS
** $Source:$
** $Author: gsl $
** $Date:$
** $Revision:$
******************************************************************************
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
int VL_vtrim(char *string);								/* Trim a string of trailing blanks.	*/
int VL_vtrimlen(char *outstr, char *instr, int length);					/* Trim a string of length.		*/
int VL_vputlen(char *outstr, char *instr, int length);					/* Opposite of vtrimlen.		*/

#endif /* vtrim_H */

/*
**	History:
**	$Log: vtrim.h,v $
**	Revision 1.3  2003/01/31 19:25:55  gsl
**	Fix copyright header
**	
**	Revision 1.2  2002/07/15 20:16:15  gsl
**	Videolib VL_ gobals
**	
**	Revision 1.1  1996/03/28 21:42:52  gsl
**	Initial revision
**	
**
*/
