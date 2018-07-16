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
**	File:		vssort.h
**
**	Purpose:	To ...
**
**
**	History:
**	mm/dd/yy	Written by ...
**
*/

#ifndef vssort_H
#define vssort_H

#include "idsistd.h"

struct select_criteria 
{
	int4 fpos;
	int4 flen;
	char ftyp;
	char testrel[2];
	char value[18];
	char connect[3];
};

struct key_info
{
	int4 spos;
	int4 len;
	char type;
	char order;
};

#define MAXSORTINFILES	20
#define	MAXSORTKEYS	8
#define MAXSORTSELECTS 	32

struct sortdata_s
{
	char	ifile[8];
	char	ilib[8];
	char	ivol[6];
	char	ofile[8];
	char	olib[8];
	char	ovol[6];
	struct 
	{
		char	pos[4];
		char	len[3];
		char	type;
		char	order;
	} keys[8];
};

extern
void vssort(	char infiles[][9], char inlibs[][9], char invols[][7], char infilenames[][80], 
		char intypes[], int4 inlens[], int4 incount,
		char *outfile, char *outlib, char *outvol, char *outfilename, 
		char outfileorg, int4 outmaxrec, char outrectype, int replace_flag,
		struct select_criteria selinfo[], int selcnt, struct key_info keyinfo[], int keycnt, int stable_flag, 
		int4 *retcode, int4 *errcode, char *errmess);

extern char *vssort_sortcode(int4 sortcode);
extern char *vssort_retcode(int4 retcode);

#endif /* vssort_H */
/*
**	History:
**	$Log: vssort.h,v $
**	Revision 1.5  1996-07-23 14:17:53-04  gsl
**	drcs update
**
**
**
*/
