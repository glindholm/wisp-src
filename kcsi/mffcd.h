/* Copyright (c) 1988-1996 DevTech Migrations, All rights reserved. */
/* $Id:$ */

#ifndef	MFFCD_H
#define	MFFCD_H

/*
 **	Includes
*/
#include "intdef.h"

#ifndef NULL
#define NULL	((void*)0)
#endif

#define CHAR_NULL	'\000'		/* Standard null character def.	*/

#ifndef FALSE
#define FALSE		0
#define TRUE		1
#endif

#define LENGIA		14
#define LENKDA		16
#define LENCDA		10

/*----
Structures for accessing MF-Cobol FCD
------*/

/*
**	Key Definition Block - Component Definition area
*/
typedef struct mfxcda
{
	char reserved[2];
	char pos[4];			/* Offset of component in the record	*/
	char len[4];			/* Length of the component		*/
} MFCDA;

/*
**	Key Definition Block - Key Definition area
*/
typedef	struct mfxkey
{
	char comp_cnt[2];		/* Component count				*/
	char offset[2];			/* Offset to 1st component def area for key	*/
	char dup_flags;			/* Key flags, bits are set			*/
	char compress;			/* Compression flags, bits are set		*/
	char sparse;			/* Sparse character				*/
	char reserved[9];
} MFKDA;

/*
**	Key Definition Block - Global Information area
*/
typedef struct mfxgia
{
	char kdb_len[2];		/* Length of Key Definition block		*/
	char reserved1[4];
	char nkeys[2];			/* Number of keys				*/
	char reserved2[6];
} MFGIA;

/*
**	File Control Description is a 100-byte area that contains info. about
**	the file in use.
*/
typedef	struct xfcd
{
	char status[2];			/* User file status				*/
	char reserved01[3];
	char org;			/* File organization				*/
	char access_mode;		/* User status and Access Mode indicators	*/
	char open_mode;			/* Open mode					*/
	char reserved02[3];
	char namelen[2];		/* Length of filename				*/
	char reserved03[11];
	char lockmode;			/* Lock mode flags for shareable files		*/
	char flags;			/* Other flags					*/
	char reserved04[2];
	char handle[4];			/* File handle - will be 32-bit ptr in COBOL	*/
	char reserved05;
	char ans_flag;			/* File status type				*/
	char format;			/* File format					*/
	char reserved06[3];
	char maxreclen[2];		/* Maximum record length			*/
	char reserved07[3];
	char rel_key[4];		/* Relative record number			*/
	char recording_mode;		/* Recording mode				*/
	char curreclen[2];		/* Current record length (in bytes)		*/
	char minreclen[2];		/* Minimum record length (in bytes)		*/
	char keynum[2];			/* Key-of-reference (indexed files)		*/
	char keylen[2];			/* Effective key length				*/
	char rec[4];			/* Pointer to the record area (4 bytes)		*/
	char filename[4];		/* Pointer to the filename area (4 bytes)	*/
	char mfkblock[4];		/* Pointer to the key definition block (4 bytes)*/
	char reserved08[4];
	char rel_byte_address[4];	/* Relative byte address			*/
	char reserved09[2];
	char comp_routine;		/* Data compression routine indicator		*/
	char share_session_id[4];	/* Fileshare V2 session-id			*/
	char share_file_id[2];		/* Fileshare V2 file-id				*/
	char reserved10[6];
	char interlanguage_locking;	/* Interlanguage locking (LOCKTYPE 1)		*/
	char share_flags;		/* Fileshare V2 flags				*/
	char config_flags;		/* Configuration flags				*/
	char reserved11;
	char collating;			/* Set bits for file functions			*/
	char idx_cache_sz;		/* Index cache size				*/
	char idx_cache_area;		/* Index cache area				*/ 
	char reserved12[2];

#ifdef DEBUG
	int4 magic;
#endif

} FCD;

/*----
Some specific values in Micro Focus File Control Description (FCD)
------*/

/* File organization */
#define MF_LINE_SEQUENTIAL	0x00
#define	MF_SEQUENTIAL		0x01
#define	MF_INDEXED		0x02
#define	MF_RELATIVE		0x03

/* Access modes */
#define MF_SEQUENTIAL_ACCESS	0x00
#define MF_RANDOM_ACCESS	0x04
#define	MF_DYNAMIC_ACCESS	0x08
/* User status is indicated by bit 7 */
#define	MF_USER_STATUS		0x80

/* Open Mode*/
#define	MF_OPEN_INPUT		0x00
#define	MF_OPEN_OUTPUT		0x01
#define	MF_OPEN_IO		0x02
#define	MF_OPEN_EXTEND		0x03
#define	MF_CLOSED		0x80

/* Lock mode flags */
#define MF_LOCK_EXCLUSIVE	0x01
#define MF_LOCK_AUTOMATIC	0x02
#define	MF_LOCK_MANUAL		0x04
#define MF_RETRYLOCK		0x08
#define MF_SKIPLOCK		0x10
#define MF_RETRYOPEN		0x20
#define MF_WRITELOCK		0x40
#define MF_LOCK_MULTIPLE	0x80

/* Other flags */
#define MF_LINE_ADVANCING	0x01
#define MF_MULTIPLE_REEL	0x02
#define MF_NODETECTLOCK		0x04
#define MF_EXTERNAL_FILE	0x10
#define MF_NOT_OPTIONAL		0x20
#define MF_OPTIONAL		0x80

/* File status type */
#define MF_ANS85		0x80

/* File format */
#define MF_FILE_DEFAULT_FORMAT	0x00
#define MF_CISAM_FORMAT		0x01
#define MF_LEVEL2_FORMAT	0x02
#define MF_NATIVE_FORMAT	0x03
#define MF_IDX4_FORMAT		0x04

/* Recording mode */
#define MF_FIXED_LENGTH		0x00
#define MF_VARIABLE_LENGTH	0x01

/* Data compression */
#define MF_NO_COMPRESSION	0x00
#define MF_CBLDC001		0x01
#define MF_USRDC128		0x80

/* Configuration flags */
#define MF_IGNORELOCK		0x01
#define MF_UPDATE_CUR_REC_PTR	0x20
#define MF_USE_RELATIVE_BA	0x40
#define MF_WRITETHRU		0x80

/* File functions / Collating */
#define MF_IGNORE_MIN_CHK	0x08
#define MF_ADV_BYTE		0x10
#define MF_WRITE_BEFORE_ADV	0x20
#define MF_WRITE_AFTER_ADV	0x40
#define MF_EBCDIC_COLLATING	0x80


/*----
For Key Definition Area
------*/

/* Key flags */
#define MF_SPARSE_KEY		0x02
#define	MF_IS_PRIMARY		0x10
#define	MF_DUPS_ALLOWED		0x40

/* Compression flags */
#define MF_COMP_DUPS		0x01
#define MF_COMP_LEADING		0x02
#define MF_COMP_TRAILING	0x04

/* Operation Codes */
#define	MFIO_OPEN_INPUT		0x00
#define	MFIO_OPEN_OUTPUT	0x01
#define	MFIO_OPEN_IO		0x02
#define MFIO_OPEN EXTEND	0x03

#define	MFIO_CLOSE		0x80
#define MFIO_CLOSE_WITH_LOCK	0x81

#define	MFIO_SEQ_READ_NO_LOCK	0x8D
#define MFIO_SEQ_READ_LOCK	0xD8
#define MFIO_SEQ_READ		0xF5

#define MFIO_READ_PREV_NO_LOCK	0x8C
#define MFIO_READ_PREV_LOCK	0xDE
#define MFIO_READ_PREV		0xF9

#define MFIO_RNDM_READ_NO_LOCK	0x8E
#define MFIO_RNDM_READ_LOCK	0xDA
#define	MFIO_RNDM_READ		0xF6

#define MFIO_DIR_READ_NO_LOCK	0x8F
#define MFIO_DIR_READ_LOCK	0xD6
#define MFIO_DIR_READ		0xC9

#define MFIO_POSITION_READ	0xF1

#define MFIO_WRITE_BEFORE	0xE1
#define MFIO_WRITE_AFTER	0xE2
#define MFIO_WRITE_BEFORE_PAGE	0xE5
#define MFIO_WRITE_AFTER_PAGE	0xE6

#define	MFIO_WRITE		0xF3
#define	MFIO_REWRITE		0xF4

#define MFIO_START_EQ_PRIM_KEY	0xE8
#define	MFIO_START_EQ		0xE9
#define MFIO_START_GT		0xEA
#define	MFIO_START_NLT		0xEB
#define	MFIO_START_LT		0xFE
#define MFIO_START_LT_EQ	0xFF

#define	MFIO_DELETE		0xF7
#define MFIO_DELETE_FILE	0xF8

#define MFIO_UNLOCK		0x0E
#define MFIO_COMMIT		0xDC
#define MFIO_ROLLBACK		0xDD

#endif	/* _MFFCD_H */

/*
**	History:
**	$Log: mffcd.h,v $
**	Revision 1.9  1997/10/30 19:33:00  scass
**	Changed pointer definitions to be 4 bytes so FCD structure
**	will always be 100 bytes as defined in MF documentation.
**	
**	Revision 1.8  1997-08-13 10:26:33-04  scass
**	Changed the defines so are all hex values.
**
**	Revision 1.7  1997-08-12 13:39:35-04  scass
**	Change to use WANG_MAX_KEYS instead of pointers
**
**	Revision 1.6  1997-08-06 10:51:25-04  scass
**	Corrected types so compatible with COBOL storage.
**
**	Revision 1.5  1997-08-01 17:29:26-04  scass
**	Added test for ifdef NULL
**
**	Revision 1.4  1997-08-01 14:17:04-04  scass
**	Correct for current work using ExtFH()
**
**	Revision 1.3  1996-09-17 19:34:14-04  gsl
**	drcs update
**
*/
