static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*	      Copyright (c) 1988,1989,1990,1991,1992,1993,1994		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
**	File:		sysdev.c
**
**	Purpose:	To get the device information from the system
**
**	Routines:	
**	osd_freeblocks()	For VMS - calls the system service sys$getdviw to and returns the
**				number of free blocks on the device.
**
**
*/

#ifdef VMS
#include <descrip.h>
#include <ssdef.h>
#include <dvidef.h>

#include "sysdev.h"
#include "idsistd.h"
#include "werrlog.h"

static struct	{
		unsigned short		buflen;						/* the length of the buffer		*/
		unsigned short	 	item_code;					/* the code for the request to GETDVI	*/
		uint4  			*bufptr;					/* a pointer to the buffer		*/
		unsigned long		*retlen;					/* the return length of the buffer	*/
		int4 		endbuf;							/* the end of the buffer		*/
	} mybuf;

/*
**	Routine:	osd_freeblocks()
**
**	Function:	Calls the system service sys$getdviw() to get the number
**			of free blocks on the device.
**
**	Description:	Pass in the device/logical to check and will do a system service
**			call and return the # of free blocks or 0 if any errors.
**
**	Arguments:
**	cur_dev		The null terminated string containing the Wang style volume
**
**	Globals:	None
**
**	Return:	
**	#		Success  (The uint4 value returned)
**	0		Failure of system srevice
**
**	Warnings:	None
**
**	History:	
**	09/14/94	Written by SMC
**
*/
uint4 osd_freeblocks(char *cur_dev)							/* Return the free blocks on device	*/
{
#define		ROUTINE		92000
	char l_device[64];
	uint4 retbuf;
	unsigned long length;
	int4 status;
	uint4 free_blocks;
#include "sysdev.d"

	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);

	strcpy(l_device,cur_dev);							/* Copy device name to local var.	*/
	c_device.dsc$w_length = strlen(l_device);

	mybuf.item_code = DVI$_FREEBLOCKS;						/* ask for the free blocks on device 	*/
	mybuf.buflen = 4;								/* in a 4 byte buffer			*/
	mybuf.bufptr = &retbuf;								/* which is retbuf			*/
	mybuf.retlen = &length;								/* return length address		*/
	mybuf.endbuf = 0;								/* mark end of buffers			*/

	status = sys$getdviw((long)0,0,&c_device,&mybuf,0,0,0,0);			/* get the free blocks on disk		*/

	if (status == SS$_NORMAL)
	{
		return(retbuf);								/* Return free blocks			*/
	}
	else return((uint4)0);								/* or 0 if not successful.		*/
}

#endif
/*
**	History:
**	$Log: sysdev.c,v $
**	Revision 1.5  1996-08-19 18:33:00-04  gsl
**	drcs update
**
**
**
*/
