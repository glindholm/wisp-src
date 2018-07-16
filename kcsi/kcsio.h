/*
******************************************************************************
**
** KCSI - King Computer Services Inc.
**
** $Id:$
**
** 
** CVS
** $Source:$
** $Author: gsl $
** $Date:$
** $Revision:$
******************************************************************************
*/


#ifndef	_KCSIO_H
#define	_KCSIO_H

#include "intdef.h"

/*----
Structures to convert the COBOL versions to 'C' versions of the structure.
Compressed and variables flags are not converted as they have no meaning
in c-isam.
The primary is saved in the keydesc struct array as entry 1.
------*/

#define WANG_MAX_KEYS		17	/* 16 alt keys + primary */
#define NPARTS			8	/* number of key parts per key */

/** KEY DEFINING OPTIONS **/
#define ISNODUPS       0               /* duplicates not allowed */
#define ISDUPS         1               /* duplicate keys permitted */
#define DCOMPRESS      2               /* compress duplicates */
#define LCOMPRESS      4               /* leading redundancy compress */
#define TCOMPRESS      8               /* trailing constant compress */
#define TNULL          0x10            /* use null as trailing constant */
#define COMPRESS ( LCOMPRESS + DCOMPRESS + TCOMPRESS )

/****************************************************************************
                         E R R O R    C O D E S
*****************************************************************************/

/** ERROR MNEMONICS **/
#define EDUPL          100             /* duplicate record */
#define ENOTOPEN       101             /* file not open */
#define EBADARG        102             /* invalid argument */
#define EBADKEY        103             /* invalid key description */
#define ETOOMANY       104             /* out of file descriptors */
#define EBADFILE       105             /* invalid isam file format */
#define ENOTEXCL       106             /* exclusive lock required */
#define ELOCKED        107             /* record claimed by another */
#define EKEXISTS       108             /* key already exists */
#define EPRIMKEY       109             /* primary key may not be used */
#define EENDFILE       110             /* beginning or end of file reached */
#define ENOREC         111             /* no match was found */
#define ENOCURR        112             /* there is no "current" established */
#define EFLOCKED       113             /* entire file locked by another */
#define EFNAME         114             /* file name too long */
#define ENOLOK         115             /* cannot create lock file */
#define EBADMEM        116             /* memory allocation request failed */
#define EBADCOLL       117             /* bad custom collating */
#define EUSER          129             /* too many users */
#define EVIRTUAL       140             /* unable to reopen virtual file */

#ifdef ISAMMAIN				/* global variable control */
#define STATUS
#else
#define STATUS extern
#endif

STATUS uint4 isrecnum;			/* record number of current record */

struct keypart                 
{
	int2 kp_start;       		/* offset to key part */
	int2 kp_leng;        		/* # of bytes in keypart */
	int2 kp_type;        		/* processing directions for keypart */
};

#define k_start k_part[0].kp_start
#define k_leng  k_part[0].kp_leng
#define k_type  k_part[0].kp_type

struct keydesc
{
	int2 k_flags;			/* key characteristics */
	int2 k_nparts;			/* number of parts in key */
	struct keypart k_part[NPARTS];	/* description of each part */
					/* the rest are INTERNAL */
	int2 k_len;			/* length of key */
	uint4 k_rootnode;		/* record number of root node */
};

typedef struct _kcsio_block{
     int2 _status;                   /* 'C' style IO error              */
     int2 _open_status;              /* '0' = closed '1' = OPEN         */
     int4 _space;                    /* ASCII numerics "99999999"       */
     int2 _record_len;               /*     ditto      "9999"           */
     int2 _io_key;                   /* key number for IO "00" - "16"   */
     int2 _last_io_key;              /* Last start or read              */
     int4 _rel_key;                  /* relative key                    */
     int2 _io_channel;               /* ASCII numeric of d-isam channel */
     int2 _altkey_count;             /* number of altkeys               */
     char _io[3];                     /* IO request                     */
     char _last_io[3];                /* Last IO request-used for start */
     char _name[9];
     char _library[9];
     char _volume[7];
     char _prname[9];
     char _sys_name[81];             /* resulting UNIX name             */
     struct keydesc _key[WANG_MAX_KEYS];
			            /* prim + 16 ALTKEY descriptions   */
     char *_io_vector;		     /* for files that use pointers     */
     char _org[2];                   /* file organization               */
     char *_record;                  /* pointer to the record           */
     char _format;                   /* file format                     */
     int4 _pos;			     /* position of last read           */
     char *_fcd;     		     /* ptr to file control descriptor  */
     char _x_status[10];             /* Extended status for MF COBOL    */
     char *_mfkblock;                /* ptr to the Key Definition Block */
     }KCSIO_BLOCK;

#define	io_is_open(x)		(x->_io[0] == 'O')
#define	file_is_indexed(x)	(x->_key[0].k_nparts > 0)

#define KFB	KCSIO_BLOCK

#endif

/*
**	History:
**	$Log: kcsio.h,v $
**	Revision 1.11  2003/02/05 15:50:11  gsl
**	Fix copyright headers
**	
**	Revision 1.10  2002/10/17 21:22:42  gsl
**	cleanup
**	
**	Revision 1.9  1997/10/30 19:32:18  scass
**	Added mfkblock pointer to KCSIO_BLOCK structure.
**	
**	Revision 1.8  1997-08-13 10:24:10-04  scass
**	Added format field for handling exntended file formats for
**	Micro Focus COBOL
**
**	Revision 1.7  1997-08-06 18:14:36-04  scass
**	changed types to be int2 instead of short
**
**	Revision 1.6  1997-08-06 15:19:53-04  scass
**	Added structures that were defined in disam.h
**	so don't need to have disam stuff.
**
**	Revision 1.5  1997-08-06 10:57:12-04  scass
**	Added include of intdef.h
**
**	Revision 1.4  1997-08-04 11:49:01-04  scass
**	Change int to int2 and long to int4
**
**	Revision 1.3  1996-09-17 19:34:10-04  gsl
**	drcs update
**
**
**
*/
