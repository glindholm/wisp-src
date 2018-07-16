/* 
	Copyright (c) 1995 DevTech Migrations, All rights reserved.
	$Id:$
*/

			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988,1989,1990,1991,1992,1993,1994		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
**	File:		sysdev.h
**
**	Purpose:	To have device routines - system dependent
**
**
**	History:
**	09/16/94	Written by SMC
**
*/

#ifdef VMS

#ifndef sysdev_H
#define sysdev_H

#include "idsistd.h"

#define OSD_BLOCK_SIZE	512

extern uint4 osd_freeblocks(char *cur_dev);						/* Return the free blocks on device	*/

#endif /* sysdev_H */

#endif
/*
**	History:
**	$Log: sysdev.h,v $
**	Revision 1.5  1996/07/23 18:17:52  gsl
**	drcs update
**	
**
**
*/
