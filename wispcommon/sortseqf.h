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


#ifndef SORTSEQF_H
#define SORTSEQF_H

#define SSF_MAX_NUMKEYS		16

/*
	Error defines for sortseqf.

*/

#define WARN_LASTLINE		1
#define WARNING			19
#define ERR_TEXT		20
#define ERR_NOINFILE		21
#define ERR_NOOUTFILE		22
#define ERR_BADRECSIZE		23
#define ERR_BADFILETYPE		24
#define ERR_BADNUMKEYS		25
#define ERR_NOSORTKEYS		26
#define ERR_BADOFFSET		27
#define ERR_BADLENGTH		28
#define ERR_BADDIRECTION	29
#define ERR_BADTYPE		30
#define	ERR_BINLEN		31
#define ERR_FLOATLEN		32
#define ERR_OPENINPUT		33
#define ERR_OPENOUTPUT		34
#define ERR_MALLOC		35
#define ERR_READ		36
#define ERR_WRITE		37
#define ERR_SIZEINT		38
#define ERR_SIZEFLOAT		39
#define ERR_NORECEND		40
#define ERR_BADSIZEEND		41
#define ERR_NORECORDS		42

struct s_sortkeys
{
	int	offset;								/* The offset into the record.			*/
	int	length;								/* The length of the key.			*/
	int	direction;							/* Ascending / Descending			*/
#define KD_ASCENDING		0
#define KD_DESCENDING		1
	int	type;								/* ASCII, BINARY, FLOAT etc.			*/
#define	KT_ASCII		0
#define	KT_BINARY		1
#define KT_FLOAT		2
#define	KT_DECIMAL		3
#define	KT_ZONED_LEAD		4
#define KT_ZONED_TRAIL		5
#define KT_ZONED_RIGHT		6
#define KT_ZONED_LEFT		7
#define KT_PACKED		8
#define KT_PACKED_UNSIGNED	9
#define KT_BINARY_NORMAL	10
#define KT_BINARY_2N2		11
#define KT_YY_PIVOTYEAR		12		/* YY PIC 99 - based on YYPIVOTYEAR */
#define KT_TYPE_COUNT		13		/* Insert new types before this one */
};

int WL_sortseqf( char *infile, char *outfile, char *tmpdir, int memsizek, char filetype, int recsize, char *recend, 
	     int numkeys, struct s_sortkeys sortkeys[], int errlevel, FILE *errfile, char *errbuff );


#endif /* SORTSEQF_H */
/*
**	History:
**	$Log: sortseqf.h,v $
**	Revision 1.14  2003/01/31 19:26:33  gsl
**	Fix copyright header
**	
**	Revision 1.13  2002/07/09 04:14:03  gsl
**	Rename global WISPLIB routines WL_ for uniqueness
**	
**	Revision 1.12  1998/09/08 20:32:26  gsl
**	Add new key value for a YY pivot year
**	
**	Revision 1.11  1996-09-26 13:13:45-04  gsl
**	Add defined SSF_MAX_NUMKEYS
**
**	Revision 1.10  1996-07-23 11:17:52-07  gsl
**	drcs update
**
**
**
*/
