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
**	File:		vssort.h
**
**	Purpose:	To ...
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

void WL_vssort(	char infiles[][9], char inlibs[][9], char invols[][7], char infilenames[][80], 
		char intypes[], int4 inlens[], int4 incount,
		char *outfile, char *outlib, char *outvol, char *outfilename, 
		char outfileorg, int4 outmaxrec, char outrectype, int replace_flag,
		struct select_criteria selinfo[], int selcnt, struct key_info keyinfo[], int keycnt, int stable_flag, 
		int4 *retcode, int4 *errcode, char *errmess);

char *WL_vssort_sortcode(int4 sortcode);
char *WL_vssort_retcode(int4 retcode);

#endif /* vssort_H */
/*
**	History:
**	$Log: vssort.h,v $
**	Revision 1.8  2003/02/05 15:40:14  gsl
**	Fix copyright headers
**	
**	Revision 1.7  2003/01/31 19:26:33  gsl
**	Fix copyright header
**	
**	Revision 1.6  2002/07/09 04:14:03  gsl
**	Rename global WISPLIB routines WL_ for uniqueness
**	
**	Revision 1.5  1996/07/23 18:17:53  gsl
**	drcs update
**	
**
**
*/
