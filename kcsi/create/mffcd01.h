/* Copyright (c) 1988-1996 DevTech Migrations, All rights reserved. */
/* $Id:$ */

#ifndef	_MFFCD_H
#define	_MFFCD_H
/*----
Structures for accessing MF-Cobol FCD
------*/

typedef	char	MFINT[2];
typedef char	MFLONG[4];

typedef struct _mfxpart{
	char reserved[2];
	MFLONG pos;
	MFLONG len;
	}MFXPART;

typedef	struct _mfxkey{
	MFINT parts;
	MFINT offset;
	char dup_flags[1];
	char compress[1];
	char sparse[1];
	char reserved[9];
	}MFXKEY;

typedef struct _mfkdef{
	MFINT	kdb_len;	/* key def block len */
	char reserved1[4];
	MFINT nkeys;
	char reserved2[6];
	}MFKDEF;

typedef struct _mfkblock{
	MFKDEF	mfkdef;
	char mfxkey[ (sizeof(MFXKEY) + sizeof(MFXPART)) * 17];
	}MFKBLOCK;
	

typedef	struct _fcd{
	char status[2];
	char reserved01[3];
	char org[1];
	char access_mode[1];
	char open_mode[1];
	char reserved02[3];
	MFINT	namelen;
	char reserved03[11];
	char lockmode[1];
	char flags[1];
	char reserved04[2];
	char handle[4];		/* See notes on handle */
	char reserved05[1];
	char ans_flag[1];
	char format[1];
	char reserved06[3];
	MFINT	maxreclen;
	char reserved07[3];
	MFLONG	rel_key;
	char recording_mode[1];
	MFINT	curreclen;
	MFINT	minreclen;
	MFINT	keynum;
	MFINT	keylen;
	char *rec;
	char *filename;
	MFKBLOCK	*mfkblock;
	char reserved08[4];
	MFLONG rel_byte_address;
	char reserved09[2];
	char comp_routine[1];
	MFLONG	share_session_id;
	MFINT	share_file_id;
	char reserved10[6];
	char interlanguage_locking[1];
	char share_flags[1];
	char config_flags[1];
	char reserved11[1];
	char collating[1];
	char reserved12[4];
	}FCD;
/*----
Some specific values in mf fcd
------*/

/*----
For _mfxkey._dups_flag
------*/

#define	MF_IS_PRIMARY		0x10
#define	MF_DUPS_IN_ORDER	0x40
#define	MF_DUPS_NOT_IN_ORDER	0x20
#define	MF_DUPS			(MF_DUPS_IN_ORDER | MF_DUPS_NOT_IN_ORDER)
/* This may change when I see the defaults */
#define	SET_MF_DUPS		MF_DUPS_NOT_IN_ORDER

/* Open Mode*/

#define	MF_CLOSED		128
#define	MF_OPEN_INPUT		0
#define	MF_OPEN_OUTPUT		1
#define	MF_OPEN_IO		2
#define	MF_OPEN_EXTEND		3

/* File org */
#define MF_LINE_SEQUENTIAL	0
#define	MF_SEQUENTIAL		1
#define	MF_INDEXED		2
#define	MF_RELATIVE		3

/* Access modes */
#define	MF_DYNAMIC_ACCESS	8

/* Misc */
#define MF_LOCK_EXCLUSIVE	0x01
#define	MF_LOCK_MANUAL		0x04
#define MF_NOT_OPTIONAL		0x20
#define MF_FIXED_LENGTH		0x00
#define MF_ANS85		0x80
#define	MF_USER_STATUS		0x80

/* io bytes */
#define	MFIO_OPEN_INPUT		0x00
#define	MFIO_OPEN_OUTPUT	0x01
#define	MFIO_OPEN_IO		0x02
#define	MFIO_CLOSE		0x80
#define	MFIO_READ_NEXT		0x8d
#define	MFIO_READ		0xf6
#define	MFIO_HOLD_NEXT		0xd8
#define	MFIO_HOLD		0xda
#define	MFIO_START_EQ		0xe9
#define	MFIO_START_NLT		0xeb
#define	MFIO_WRITE		0xf3
#define	MFIO_REWRITE		0xf4
#define	MFIO_DELETE		0xf7
#define	MFIO_READ_PREVIOUS	0xf9
#define	MFIO_START_GT		0xfe


/* Special IO routines */

#define	MFIO_FILE_INFO		0x06

#endif	/* _MFFCD_H */

/*
**	History:
**	$Log: mffcd01.h,v $
**	Revision 1.3  1996-09-17 19:34:14-04  gsl
**	drcs update
**
**
**
*/
