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
**	File:		vchinese.h
**
**	Project:	video/lib
**
**	RCS:		$Source:$
**
**	Purpose:	Chinese character support.
**
*/

#ifndef vchinese_H
#define vchinese_H
/*
**	Includes
*/

/*
**	Structures and Defines
*/

#define MAX_CODENAME_LEN 10
#define MAX_FILENAME_LEN 128

#define CS_IVS_CH 0
#define CS_GW 1
#define CS_BIG5 2
#define CS_HP 3
#define CS_CCDC 4
#define CS_ET 5

#define CS_TYPE_MAX 6

#define XL_F2_TO_F1 1
#define XL_F1_TO_F2 2

#define XL_TO_IVS    XL_F2_TO_F1
#define XL_FROM_IVS  XL_F1_TO_F2

#define ORDER_NORMAL 1
#define ORDER_REVERSE 2

struct xlat_s 
{
	char *name;
	int bytesper;
};

struct xfileheader
{
	int magic, version;
	int fmt1, fmt2;
	int f1_ind_offs, f1_ind_len;
	int f2_ind_offs, f2_ind_len;
	int f1_to_f2_offs, f1_to_f2_len; 
	int f2_to_f1_offs, f2_to_f1_len; 
};

#define HOLDSZ 10

struct xlcontext
{
	unsigned char *indbuf;
	unsigned char *xlatbuf;
	int *multvalues;
	int srcsize,dstsize;
	int multipos;
	int hold[HOLDSZ];
	int holdpos, holdstart;
};

#define CHINESEMAGIC 0xd5dea100 
#define CHINESEVERSION 1
#define CHTABLEPATH "WLANGUAGEPATH"
#define WISPLANGVAR "WISPLANG"

#ifdef IVS__CHINESE__

/* these are general IVS_xlat_stream contexted used by video (vrawunix) for all I/O */
struct xlcontext *IVS_outctx=NULL, *IVS_inctx=NULL;
	
#else /* ! IVS__CHINESE__ */

extern struct xlcontext *IVS_outctx, *IVS_inctx;

#endif /* ! IVS__CHINESE__ */

/*
**	Function Prototypes
*/
void IVS_xlat_stream(unsigned char *instr, int insz, unsigned char *out, int* outsz, struct xlcontext **ctx);

#endif /* vchinese_H */

/*
**	History:
**	$Log: vchinese.h,v $
**	Revision 1.10  2003/01/31 19:38:02  gsl
**	Fix -Wall warnings
**	
**	Revision 1.9  2003/01/31 19:25:56  gsl
**	Fix copyright header
**	
**	Revision 1.8  2002/07/15 13:29:02  gsl
**	IVS_ globals
**	
**	Revision 1.7  2002/07/12 20:40:43  gsl
**	Global unique WL_ changes
**	
**	Revision 1.6  1995/04/25 09:50:24  gsl
**	drcs state V3_3_15
**	
 * Revision 1.5  1995/04/17  11:44:02  gsl
 * drcs state V3_3_14
 *
 * Revision 1.4  1995/04/10  09:10:02  gsl
 * fixed compiler warnings and added headers
 *
**
*/
