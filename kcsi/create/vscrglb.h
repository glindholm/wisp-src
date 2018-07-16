/* Copyright (c) 1988-1996 DevTech Migrations, All rights reserved. */
/* $Id:$ */

#ifndef _VSCRGLB_H
#define _VSCRGLB_H

#include "kcsio.h"
#include "ll.h"
#include "iocode.h"
#include "gp.h"
#include "create.h"
#include "kcsifunc.h"

typedef struct{
	LL	ll;
	int type;
	int second;		/* true when an earlier field names this file*/
	int outpos;
	int len;
	char string[65];
	char endrange[65];
	long begval;
	long endval;
	long curval;
	long increment;
	int keylen;
	int repeat;
	int inpos;
	int cinpos;
	int clen;
	int count;
	int counter;
	char compare[3];
	char cstring[65];
	KCSIO_BLOCK	*kfb;
	}CR_FLD;

/*----
Different field types
------*/

#define	CFIELD_FILE		1
#define	CFIELD_PAD		2
#define	CFIELD_STRING		3
#define	CFIELD_SEQUENCE		4

/*----
Aliases that for struct members that do double duty
------*/

#define	begrange	string

typedef struct {
	LL	ll;
	int	no;
	CR_FLD	*fld;
	long	count;
	long	counter;
	}CR_BLK;

typedef	struct {
	KCSIO_BLOCK	ofile;
	long records;
	long errors;
	}CR_OUT;

#ifdef EXTERN_DEF
#undef EXTERN_DEF
#endif

#ifdef _VSCRGLB_C
#define	EXTERN_DEF
#else
#define	EXTERN_DEF	extern
#endif

EXTERN_DEF	CR_OUT	cr_out;
EXTERN_DEF	CR_BLK	*cr_blk;
EXTERN_DEF	char	create_inlib[9];
EXTERN_DEF	char	create_invol[7];
EXTERN_DEF	char	create_outlib[9];
EXTERN_DEF	char	create_outvol[7];
EXTERN_DEF	int 	cr_errlist;
EXTERN_DEF	int	cr_output_org;
EXTERN_DEF	char	cr_out_rec[2049];
/*
EXTERN_DEF	char	cr_in_rec[2049];
*/
EXTERN_DEF	KCSIO_BLOCK	wkfb;
EXTERN_DEF	CR_FLD		wfld;
EXTERN_DEF	char		wrecord[2049];
EXTERN_DEF	int		cr_split_keys;
EXTERN_DEF	int		keys_overlap;
EXTERN_DEF	int		cr_debug;


void *ll_select(void *base, int (*func) ());
int block_is_done(CR_BLK *blk);

#endif /* _VSCRGLB_H */

	
	

/*
**	History:
**	$Log: vscrglb.h,v $
**	Revision 1.3  1997-10-02 10:44:48-04  gsl
**	Fix warnings
**
**	Revision 1.2  1996-10-02 12:10:11-04  gsl
**	Add standard headers
**	Fix prototypes
**
**
**
*/
