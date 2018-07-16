/* Copyright (c) 1988-1996 DevTech Migrations, All rights reserved. */
/* $Id:$ */
/*----
Some wisp constants
for the mode
------*/

#define	WISP_OUTPUT	0x00000001
#define	WISP_PRINTFILE	0x00000002
#define	WISP_SCRATCH	0x00000004
#define	WISP_SUBMIT	0x00000008
#define	WISP_IO		0x00000010
#define	WISP_SORT	0x00000020
#define	WISP_LIB	0x00000040
#define	WISP_NORESPEC	0x00000080
#define	WISP_INDEXED	0x00000100
#define	WISP_CASESEN	0x00000200
#define	WISP_NOWRITE	0x00000400
#define	WISP_PRNAME	0x00000800
#define	WISP_BACKFILL	0x00001000
#define	WISP_SEQSEQ	0x00002000
#define	WISP_GETPARM	0x00004000
#define	WISP_NODISPLAY	0x00008000
#define	WISP_SEQDYN	0x00010000
#define	WISP_WORK	0x00020000
#define	WISP_TEMP	0x00040000
#define	WISP_ERROR	0x00080000
#define	WISP_DECLARE	0x00100000
#define	WISP_EXTEND	0x00200000

#define WISP_DBFILE	0x00800000


/*
**	History:
**	$Log: kwisp.h,v $
**	Revision 1.4  1997-07-09 17:00:05-04  scass
**	Added define for WISP_DBFILE so will know that
**	this bit is used to set if is a database file.
**
**	Revision 1.3  1996-09-17 19:34:13-04  gsl
**	drcs update
**
**
**
*/
