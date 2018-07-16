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
**	File:		sortcall.c
**
**	Purpose:	To ...
**
**	Routines:	
**	SORTCALL()	Wang VSSUB SORTCALL.
**
**
*/

#include "idsistd.h"
#include "idsisubs.h"
#include "wcommon.h"
#include "werrlog.h"
#include "wisplib.h"

#define		ROUTINE		61000

#ifdef VMS
#include "wfname.h"
#include "vssort.h"
#include "vmssort.h"

void SORTCALL(struct sortdata_s *sortdata, int4 *retcode)
{
	char 	flist[MAXSORTINFILES][9]; 
	char 	llist[MAXSORTINFILES][9];
	char 	vlist[MAXSORTINFILES][7];
	char 	infilename[MAXSORTINFILES][80];
	int4 	infile_count;
	char 	ofile[9];
	char 	olib[9];
	char 	ovol[9];
	char 	outfilename[80];
	char 	ofileorg;
	int4 	maxrec;
	char 	outrectype;
	int 	replace_flag;
	struct select_criteria selinfo[MAXSORTSELECTS];
	int4 	selcnt;
	struct key_info keyinfo[MAXSORTKEYS];
	int4 	keycnt;
	int4 	stable; 
	int4 	wangcode;
	int4 	errcode; 
	char 	errmess[256];
	int4	mode;
	char	*ptr;
	int	idx;

	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);						/* Say we are here.			*/

	infile_count = 1;
	memcpy(flist[0], sortdata->ifile, 8);
	flist[0][8] = (char)0;
	memcpy(llist[0], sortdata->ilib, 8);
	llist[0][8] = (char)0;
	memcpy(vlist[0], sortdata->ivol, 6);
	vlist[0][6] = (char)0;

	mode = 0;
	ptr = wfname(&mode, sortdata->ivol, sortdata->ilib, sortdata->ifile, infilename[0]);
	*ptr = (char)0;

	if (!wfexists(sortdata->ifile, sortdata->ilib, sortdata->ivol)) 
	{
		swap_put(retcode, (int4)4);
		return;
	}

	memcpy(ofile, sortdata->ofile, 8);
	ofile[8] = (char)0;
	memcpy(olib, sortdata->olib, 8);
	olib[8] = (char)0;
	memcpy(ovol, sortdata->ovol, 6);
	ovol[6] = (char)0;

	mode = IS_OUTPUT | IS_BACKFILL;
	ptr = wfname(&mode, sortdata->ovol, sortdata->olib, sortdata->ofile, outfilename);
	*ptr = (char)0;

	ofileorg = 'C';
	maxrec = 0;
	outrectype = 'F';

	replace_flag = (0==memcmp(sortdata->ifile,sortdata->ofile,8) &&
			0==memcmp(sortdata->ilib,sortdata->olib,8) &&
			0==memcmp(sortdata->ivol,sortdata->ovol,6)     ) ? 1 : 0;

	memset(&selinfo,' ',sizeof(selinfo));
	selcnt = 0;

	memset(&keyinfo,' ',sizeof(keyinfo));
	keycnt = 0;
	for(idx=0; idx<8; idx++)
	{
		if (0==memcmp(sortdata->keys[idx].pos,"    ",4))
		{
			break;
		}

		if (field2int4(sortdata->keys[idx].pos, 4, &keyinfo[idx].spos))
		{
			swap_put(retcode, (int4)16);
			return;
		}

		if (field2int4(sortdata->keys[idx].len, 3, &keyinfo[idx].len))
		{
			swap_put(retcode, (int4)16);
			return;
		}

		keyinfo[idx].type = sortdata->keys[idx].type;
		keyinfo[idx].order = sortdata->keys[idx].order;

		keycnt += 1;
	}

	if (keycnt < 1 || keycnt > 8)
	{
		swap_put(retcode, (int4)16);
		return;
	}

	stable = 0;

	vmssort(flist, llist, vlist, infilename, infile_count,
		ofile, olib, ovol, outfilename, 
		ofileorg, maxrec, outrectype, replace_flag,
		&selinfo, selcnt, &keyinfo, keycnt, stable, 
		&wangcode, &errcode, errmess);

	swap_put(retcode,wangcode);
	return;
}
#endif /* VMS */

#ifndef VMS

/*
	SORTCALL	For UNIX and MS-DOS this routine will call WISPSORT.  It will use the info from SORTINFO
			for the extra arguments.
*/
void SORTCALL(sortdata,retcode)
char	*sortdata;
int4	*retcode;
{
	char	filetype;
	int4	recsize;
	int4	*sortcode_ptr;

	werrlog(ERRORCODE(1),0,0,0,0,0,0,0,0);						/* Say we are here.			*/

	getsortinfo(&filetype, &recsize, &sortcode_ptr);

	WISPSORT(sortdata,&filetype,&recsize,sortcode_ptr,retcode);
	return;
}
#endif /* NOT VMS */

/*
**	History:
**	$Log: sortcall.c,v $
**	Revision 1.10  1996-08-19 18:32:57-04  gsl
**	drcs update
**
**
**
*/
