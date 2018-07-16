/*
******************************************************************************
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
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
**	File:		platsubs.h
**
**	Project:	WISP/LIB
**
**	RCS:		$Source:$
**
**	Purpose:	Routines for identifing the platform
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
#define PLATFORM_CODE_LEN	2
#define PLATFORM_NAME_MAX	20
#define PLATFORM_DEFINE_MAX	20

/*
**	Function Prototypes
*/
int 	WL_valplat(char code[PLATFORM_CODE_LEN]);
int	WL_plat_code(char code[PLATFORM_CODE_LEN], char* name, int* num);
void 	WL_putplattab(void);

int WL_platform_number(void);
const char* WL_platform_name(void);
const char* WL_platform_define(void);
const char* WL_platform_code(void);


#endif /* platsubs_H */

/*
**	History:
**	$Log: platsubs.h,v $
**	Revision 1.6  2003/02/07 18:29:19  gsl
**	remove old prototype
**	
**	Revision 1.5  2003/02/07 17:55:21  gsl
**	Rework the platform routines and add AIX HPUX SOLARIS 64-bit
**	
**	Revision 1.4  2003/01/31 19:26:33  gsl
**	Fix copyright header
**	
**	Revision 1.3  2002/07/09 04:14:04  gsl
**	Rename global WISPLIB routines WL_ for uniqueness
**	
**	Revision 1.2  1996/07/24 22:57:53  gsl
**	Move from wisp/lib to wisp/common
**	
**	Revision 1.1  1996-06-28 14:45:26-07  gsl
**	Initial revision
**
**
**
*/
