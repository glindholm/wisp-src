			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
**	wexith.c
*/

#include "que_jobs.h"
#include "wperson.h"
#include "wfiles.h"
#include "wglobals.h"
#include "filext.h"
#include <v/video.h>

static char rcval[4];									/* The place to put the return code.	*/

static int already = 0;									/* Already done?			*/

void wexith()										/* This is the WISP exit handler.	*/
{											/* It is installed by WFOPEN, and refed	*/
	int i;										/* vuserexit.				*/
	char fname[NAME_LENGTH+10];
	long status;

	if (already) return;								/* Already been here.			*/
	else	already = 1;

	if (flist)									/* if any files were opened for scratch	*/
	{
		flptr = flist;
		do
		{
			i = 0;
			do
			{
				fname[i] = flptr->name[i];				/* copy the file name			*/
				i++;
			} while ((flptr->name[i] != ' ') && (i < NAME_LENGTH));
			fname[i] = '\0';
			strcat(fname,";");						/* add a semicolon			*/
			delete(fname);							/* try to delete it			*/
			flptr = (fstruct *)flptr->nextfile;
		} while (flptr);
	}

	wfclose("*");									/* Ask close routine to spool everything.*/

#ifdef VMS
	setretcode(WISPRETURNCODE);
	cleanup_shrfil();								/* Cleanup the Share mem file.		*/
#endif

	if (LINKPARM)								/* If we came in from a LINK then don't clear	*/
	{
#ifdef VMS
		VMSPARGS();
#endif
		vonexit(NORMALIZE);
	}
}

