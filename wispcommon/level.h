/*
******************************************************************************
** Copyright (c) 1994-2003, NeoMedia Technologies, Inc. All Rights Reserved.
**
** WISP - Wang Interchange Source Processor
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
******************************************************************************
*/


/*
**	File:		level.h
**
**	Purpose:	Header for level.c
**
**
**	History:
**	08/19/94	Written by GSL
**
*/

#ifndef LEVEL_H
#define LEVEL_H

#define linklevel	WL_linklevel
#define newlevel	WL_newlevel
#define oldlevel	WL_oldlevel
#define zerolevel	WL_zerolevel
#define setlevel	WL_setlevel

extern int WL_linklevel(void);
extern int WL_newlevel(void);
extern int WL_oldlevel(void);
extern int WL_zerolevel(void);
extern int WL_setlevel(int level);

#endif /* LEVEL_H */
/*
**	History:
**	$Log: level.h,v $
**	Revision 1.8  2003/01/31 19:26:33  gsl
**	Fix copyright header
**	
**	Revision 1.7  2002/07/11 15:21:45  gsl
**	Fix WL_ globals
**	
**	Revision 1.6  2002/07/09 04:14:04  gsl
**	Rename global WISPLIB routines WL_ for uniqueness
**	
**	Revision 1.5  1996/07/23 18:17:48  gsl
**	drcs update
**	
**
**
*/
