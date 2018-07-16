static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991		*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
**	ACUCOBOL is defined for unix and MSDOS (when DACU defined).
*/

#if defined(unix) || defined(MSDOS) || defined(WIN32)
#define ACUCOBOL
#endif

#include "idsistd.h"

#ifdef ACUCOBOL

#include <stdio.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/types.h>
#include <string.h>

#if defined(MSDOS) || defined(WIN32)
#include <io.h>
#include <process.h>
#endif

#ifndef O_BINARY
#define O_BINARY 0
#endif
#ifndef O_TEXT
#define O_TEXT 0
#endif

#include "wisplib.h"
#include "wfvision.h"
#include "werrlog.h"

#ifdef SCO
/*	XENIX_386  is used by ACUCOBOL for SC0 386 machines */
#define XENIX_386
#endif /* SCO */

#include "visint.h"

#define ACUMAGIC_LEN	6	/* magic + version */

struct vision3_struct
{
	char	magic[4];			/*	0	4	1-4	*/
	char	version[2];			/*	4	2	5-6	*/
	char	blk_mult[2];			/*	6	2	7-8	*/
	char	pre_alloc[2];			/*	8	2	9-10	*/
	char	extension[2];			/*	10	2	11-12	*/
	char	filesize[4];			/*	12	4	13-16	*/
	char	next_blk[4];			/*	16	4	17-20	*/
	char	next_rec[4];			/*	20	4	21-24	*/
	char	first_rec[4];			/*	24	4	25-28	*/
	char	free_rec[4];			/*	28	4	28-32	*/
	char	root_vers[4];			/*	32	4	33-36	*/
	char	free_node[4];			/*	36	4	37-40	*/
	char	rec_count[4];			/*	40	4	41-44	*/
	char	del_rec[4];			/*	44	4	45-48	*/
	char	next_uniq[4];			/*	48	4	49-52	*/
	char	intern_vers[4];			/*	52	4	53-56	*/
	char	collate[4];			/*	56	4	57-60	*/
	char	node_usage[4];			/*	60	4	61-64	*/
	char	free_fails[2];			/*	64	2	65-66	*/
	char	open_cnt[2];			/*	66	2	67-68	*/
	char	nodes_used[2];			/*	68	2	69-70	*/
	char	nodes_free[2];			/*	70	2	71-72	*/
	char	rec_overhead[2];		/*	72	2	73-74	*/
	char	num_dups[2];			/*	74	2	75-76	*/
	char	_reserved1_[20];		/*	76	20	77-96	*/
	char	max_rec[2];			/*	96	2	97-98	*/
	char	min_rec[2];			/*	98	2	99-100	*/
	char	num_keys[1];			/*	100	1	101	*/
	char	compress[1];			/*	101	1	102	*/
	char	encrypt[1];			/*	102	1	103	*/
	char	max_key[1];			/*	103	1	104	*/
	char	_reserved2_[24];		/*	104	24	105-128	*/
	char	comment[32];			/*	128	32	129-160	*/
	char	keys[352];			/*	160	352	161-512	*/
};

/*
**	Vision 4 - data file
**
**	char	rec_count[4];				52	4	53-56
**	char	del_rec[4];				56	4	57-60
**	
**	char	max_rec[2];				116	2	117-118
**	char	min_rec[2];				118	2	119-120
**	char	num_keys[1];				120	1	121	
**	char	compress[1];				121	1	122
**	char	encrypt[1];				122	1	123
**	char	max_key[1];				123	1	124
**
*/
#define V4_REC_COUNT_OFF	52
#define V4_MAX_REC_OFF		116
#define V4_MIN_REC_OFF		118
#define V4_COMPRESS_OFF		121

/*
**	ROUTINE:	acuvision()
**
**	FUNCTION:	Get info about a vision file.
**
**	DESCRIPTION:	Read the vision file to get info about it.
**
**	ARGUMENTS:	
**	path		The vision file path.
**	code		The code for the info requested.
**			"RC"	Record Count  - Number of records in the file
**			"RS"	Record Size   - The maximum record length
**			"RL"	(same as "RS")
**			"FT"	File Type     - "I" indexed, "C" consecutive
**			"RT"	Record Type   - "F" Fixed, "V" variable, "C" compressed
**	raw_field	The receiver. 
**			For RC,RS,RL this is an int4.
**			For FT, RT this is a char.
**
**	GLOBALS:	None
**
**	RETURN:		
**	0		Success
**	40		Invalid code
**	44		Unable to open or read file
**
**	WARNINGS:	?
**
*/
int acuvision( const char* path, const char* code, void* raw_field )	/* ACUCOBOL Vision file system interface		*/
{
	struct	_phys_hdr phdr;						/* Struct for first header record.			*/
	struct	_log_hdr  lhdr;						/* Struct for second header record.			*/
	struct	vision3_struct v3;					/* Vision3 struct					*/
	char	v4[512];
	int	f;							/* File handle						*/
	int	i0, i1, i2;
	int4	*size;
	char	*field;
	int	vision;
	char	buff[256];
	short	tshort;

	vision = 0;

	size = (int4 *) raw_field;
	field = (char*) raw_field;

	f = open( path, O_RDONLY );					/* Open the file					*/

	if ( f == -1 )
	{
		return( 44 );
	}

	i0 = read( f, buff, ACUMAGIC_LEN);				/* Read the magic number plus version			*/
	if (i0 == -1)
	{
		close(f);
		return(44);
	}

	if (ACUMAGIC_LEN == i0)
	{
		if (0==memcmp(buff,"\x10\x12\x14\x19\x00\x04", ACUMAGIC_LEN))
		{
			vision = 4;

			/* read the rest of the header		*/
			i1 = read( f, &v4[ACUMAGIC_LEN], sizeof(v4)-ACUMAGIC_LEN );
		}
		else if (0==memcmp(buff,"\x10\x12\x14\x16\x00\x03", ACUMAGIC_LEN))
		{
			vision = 3;

			/* read the rest of the header		*/
			i1 = read( f, &((char *) &v3)[ACUMAGIC_LEN], sizeof(v3)-ACUMAGIC_LEN );
		}
		else
		{
			memcpy((char *)&phdr,buff,ACUMAGIC_LEN);
			if ( phdr.magic == 0x10121416 || phdr.magic == 0x16141210 ) 
			{
				vision = 2;
				/* read the rest of the header		*/
				i1 = read( f, &((char *) &phdr)[ACUMAGIC_LEN], sizeof(phdr)-ACUMAGIC_LEN );

				i2 = read( f, (char *) &lhdr, sizeof( lhdr ) );		/* read the second header		*/

			}
		}
	}

	close(f);
	f = -1;

	if ( memcmp( code, "RC", 2 ) == 0 )				/* If looking for record count				*/
	{
		if ( 4 == vision )
		{
			if ( !bytenormal() ) reversebytes(&v4[V4_REC_COUNT_OFF],4);
			memcpy(size,&v4[V4_REC_COUNT_OFF],4);
		}
		else if ( 3 == vision )
		{
			if ( !bytenormal() ) reversebytes(v3.rec_count,4);
			memcpy(size,v3.rec_count,4);
		}
		else if ( 2 == vision )
		{
			*size = phdr.total_records;
		}
		else
		{
			*size = (i0 >= 1) ? 1 : 0;	/* return 1 or 0					*/
		}
		
		wtrace("ACUVISION","RC","VISION(%d) File=%s RecordCount=%d", vision, path, *size);
		
		return( 0 );
	}

	if ( memcmp( code, "RS", 2 ) == 0 ||				/* If looking for record size/lenght			*/
	     memcmp( code, "RL", 2 ) == 0    )
	{
		if ( 4 == vision )
		{
			if ( !bytenormal() ) reversebytes(&v4[V4_MAX_REC_OFF],2);
			memcpy(&tshort,&v4[V4_MAX_REC_OFF],2);
			*size = tshort;
		}
		else if ( 3 == vision )
		{
			if ( !bytenormal() ) reversebytes(v3.max_rec,2);
			memcpy(&tshort,v3.max_rec,2);
			*size = tshort;
		}
		else if ( 2 == vision ) 
		{
			*size = lhdr.max_rec_size;
		}
		else
		{
			*size = 256;
		}
		
		wtrace("ACUVISION","RS","VISION(%d) File=%s RecordSize=%d", vision, path, *size);
		return( 0 );
	}

	if ( memcmp( code, "FT", 2 ) == 0 )				/* If looking for file type				*/
	{
		if ( vision > 0 )
		{
			*field = 'I';
		}
		else
		{
			*field = 'C';
		}

		wtrace("ACUVISION","FT","VISION(%d) File=%s FileType=%c", vision, path, *field);
		return( 0 );
	}

	if ( memcmp( code, "RT", 2 ) == 0 )				/* If looking for record type				*/
	{
		if (4 == vision)
		{
			if ( memcmp(&v4[V4_MAX_REC_OFF],&v4[V4_MIN_REC_OFF],2) == 0 ) *field = 'F';
			else					    *field = 'V';

			if ( v4[V4_COMPRESS_OFF] ) *field = 'C';
		}
		else if (3 == vision)
		{
			if ( memcmp(v3.max_rec,v3.min_rec,2) == 0 ) *field = 'F';
			else					    *field = 'V';

			if ( v3.compress[0] ) *field = 'C';
		}
		else if ( 2 == vision ) 
		{
			if ( lhdr.var_recsize ) *field = 'V';
			else			*field = 'F';

			if ( lhdr.compressed ) 	*field = 'C';
		}
		else
		{
			*field = 'V';
		}
		
		wtrace("ACUVISION","RT","VISION(%d) File=%s RecordType=%c", vision, path, *field);
		return( 0 );
	}

	return( 40 );
}

/*
**	ROUTINE:	visionversion()
**
**	FUNCTION:	Returns the Vision version for this file
**
**	DESCRIPTION:	Opens the file and reads the magic number.
**
**	ARGUMENTS:	
**	filename	the vision file
**
**	GLOBALS:	None
**
**	RETURN:		
**	-1		Unable to open or read the file
**	0		Not a vision file
**	1		Seems to be a vision file but unknow version.
**	2		Vision 2
**	3		Vision 3
**	4		Vision 4
**
**	WARNINGS:	None
**
*/
int visionversion(const char *filename)
{
	int	fh;
	char	buff[20];
	int	rc;

	fh = open(filename,O_RDONLY|O_BINARY,0);
	if ( fh == -1 )
	{
		return -1;
	}
	rc = read(fh,buff,ACUMAGIC_LEN);
	close(fh);
	if (rc != ACUMAGIC_LEN)
	{
		return -1;
	}

	if ( 0 == memcmp(buff, "\x10\x12\x14\x18\x00\x04", ACUMAGIC_LEN))
	{
		return 4;
	}
	if ( 0 == memcmp(buff, "\x10\x12\x14\x19\x00\x04", ACUMAGIC_LEN))
	{
		return 4;
	}
	if ( 0 == memcmp(buff, "\x10\x12\x14\x16\x00\x03", ACUMAGIC_LEN))
	{
		return 3;
	}
	if ( 0 == memcmp(buff, "\x10\x12\x14\x16\x00\x02", ACUMAGIC_LEN))
	{
		return 2;
	}
	if ( 0 == memcmp(buff, "\x10\x12\x14\x16", 4))
	{
		return 1;
	}
	if ( 0 == memcmp(buff, "\x10\x12\x14\x18", 4))
	{
		return 1;
	}
	if ( 0 == memcmp(buff, "\x10\x12\x14\x19", 4))
	{
		return 1;
	}

	return 0;
}

#else /* !ACUCOBOL */
int visionversion(const char *filename)
{
	werrlog(102,"visionversion() not implemented",0,0,0,0,0,0,0);
	return -1;
}
#endif /* !ACUCOBOL */



/*
**	History:
**	$Log: wfvision.c,v $
**	Revision 1.12  1998-05-18 10:15:28-04  gsl
**	Add support for vision 4 files
**
**	Revision 1.11  1997-03-12 13:24:10-05  gsl
**	changed to use WIN32 define
**
**	Revision 1.10  1996-08-19 19:13:49-04  gsl
**	Correct error introduced in 1.7
**
**	Revision 1.9  1996-08-19 15:33:16-07  gsl
**	drcs update
**
**
**
*/
