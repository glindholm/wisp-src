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

#if defined(AIX) || defined(HPUX) || defined(SOLARIS) || defined(LINUX)
#define _LARGEFILE64_SOURCE
#define USE_FILE64
#endif

#include "idsistd.h"

#include <stdio.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/types.h>
#include <string.h>

#ifdef WIN32
#include <io.h>
#include <process.h>
#endif

#ifndef O_BINARY
#define O_BINARY 0
#endif
#ifndef O_TEXT
#define O_TEXT 0
#endif

#if defined(WIN32)
#define O_LARGEFILE 0
#endif

#include "wisplib.h"
#include "wfvision.h"
#include "werrlog.h"
#include "wispcfg.h"
#include "assert.h"

#ifdef SCO
/*	XENIX_386  is used by ACUCOBOL for SC0 386 machines */
#define XENIX_386
#endif /* SCO */

#include "visint.h"

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

#define VISION_HEADER_SIZE	512

#define V4_REC_COUNT_OFF	52
#define V4_MAX_REC_OFF		116
#define V4_MIN_REC_OFF		118
#define V4_NUM_KEYS_B		120
#define V4_COMPRESS_OFF		121

/*
**	ROUTINE:	visioninfo()
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
int visioninfo( const char* path, const char* code, void* raw_field )
{
	char	header[VISION_HEADER_SIZE];
	int	header_len;
	int	f;							/* File handle						*/

	f = open( path, O_RDONLY | O_BINARY | O_LARGEFILE );		/* Open the file					*/

	if ( f == -1 )
	{
		return READFDR_RC_44_IO_ERROR;
	}

	header_len = read( f, header, sizeof(header));
	if (header_len == -1)
	{
		close(f);
		return READFDR_RC_44_IO_ERROR;
	}

	close(f);
	f = -1;

	return visioninfo_header(path, code, raw_field, header, header_len);
}

int visioninfo_header( const char* path, const char* code, void* raw_field, const char* raw_header, int header_len )
{
	union
	{
		struct
		{
			struct	_phys_hdr phdr;				/* Vision 2 Struct for first header record.		*/
			struct	_log_hdr  lhdr;				/* Vision 2 Struct for second header record.	       	*/
		} v2;
		struct	vision3_struct v3;				/* Vision 3 struct					*/
		char	v4[VISION_HEADER_SIZE];				/* Vision 4 header					*/
	} header;

	int4	*size;
	char	*field;
	int	vision;
	short	tshort;

	vision = 0;

	size = (int4 *) raw_field;
	field = (char*) raw_field;

	if (header_len < VISION_HEADER_SIZE)
	{
		wtrace("VISIONINFO","NOHEADER","File=%s header_len=%d (less then VISION_HEADER_SIZE=%d)",  
		       path, header_len, VISION_HEADER_SIZE);		

		return READFDR_RC_24_NO_FILE_HEADER;
	}

	memcpy((char*)&header, raw_header, VISION_HEADER_SIZE);

	if (0==memcmp(raw_header, VISION4D_MAGIC, VISION_MAGIC_LEN)) /* We are looking at the data file vs the ".vix" index */
	{
		vision = 4;
	}
	else if (0==memcmp(raw_header, VISION3_MAGIC, VISION_MAGIC_LEN))
	{
		vision = 3;
	}
	else if (0==memcmp(raw_header, VISION2BE_MAGIC, VISION_MAGIC_LEN) ||
		 0==memcmp(raw_header, VISION2LE_MAGIC, VISION_MAGIC_LEN)   )
	{
		vision = 2;
	}
	else
	{
		wtrace("VISIONINFO","UNKNOWN","File=%s header_len=%d raw_header=0x%6.6X",  
		       path, header_len, raw_header);		

		return READFDR_RC_68_UNKNOWN_FILE_FORMAT;
	}

	if ( memcmp( code, "IX", 2 ) == 0 )				/* File Index Type */
	{
		*field = 'V';	/* VISION */
		return( READFDR_RC_0_SUCCESS );
	}

	if ( memcmp( code, "IV", 2 ) == 0 )				/* File Index Type Version */
	{
		/*
		**	V2	Vision 2
		**	V3	Vision 3
		**	V4	Vision 4
		*/
		if ( 2 == vision )
		{
			field[0] = 'V';
			field[1] = '2';
			return( READFDR_RC_0_SUCCESS );
		}
		if ( 3 == vision )
		{
			field[0] = 'V';
			field[1] = '3';
			return( READFDR_RC_0_SUCCESS );
		}
		if ( 4 == vision )
		{
			field[0] = 'V';
			field[1] = '4';
			return( READFDR_RC_0_SUCCESS );
		}
	}
	
	if ( memcmp( code, "RC", 2 ) == 0 )				/* If looking for record count				*/
	{
		if ( 4 == vision )
		{
			if ( !bytenormal() ) reversebytes(&header.v4[V4_REC_COUNT_OFF],4);
			memcpy(size,&header.v4[V4_REC_COUNT_OFF],4);
		}
		else if ( 3 == vision )
		{
			if ( !bytenormal() ) reversebytes(header.v3.rec_count,4);
			memcpy(size, header.v3.rec_count, 4);
		}
		else if ( 2 == vision )
		{
			*size = header.v2.phdr.total_records;
		}
		else
		{
			return READFDR_RC_68_UNKNOWN_FILE_FORMAT;
		}
		
		wtrace("VISIONINFO","RC","VISION(%d) File=%s RecordCount=%d", vision, path, *size);		
		return( READFDR_RC_0_SUCCESS );
	}

	if ( memcmp( code, "RS", 2 ) == 0 ||				/* If looking for record size/lenght			*/
	     memcmp( code, "RL", 2 ) == 0    )
	{
		if ( 4 == vision )
		{
			if ( !bytenormal() ) reversebytes(&header.v4[V4_MAX_REC_OFF],2);
			memcpy(&tshort,&header.v4[V4_MAX_REC_OFF],2);
			*size = tshort;
		}
		else if ( 3 == vision )
		{
			if ( !bytenormal() ) reversebytes(header.v3.max_rec,2);
			memcpy(&tshort,header.v3.max_rec,2);
			*size = tshort;
		}
		else if ( 2 == vision ) 
		{
			*size = header.v2.lhdr.max_rec_size;
		}
		else
		{
			return READFDR_RC_68_UNKNOWN_FILE_FORMAT;
		}
		
		wtrace("VISIONINFO","RS","VISION(%d) File=%s RecordSize=%d", vision, path, *size);
		return( READFDR_RC_0_SUCCESS );
	}

	if ( memcmp( code, "FT", 2 ) == 0 )				/* If looking for file type				*/
	{
		int num_keys = 1;
		
		if ( 4 == vision )
		{
			num_keys = header.v4[V4_NUM_KEYS_B];
		}
		else if ( 3 == vision )
		{
			num_keys = header.v3.num_keys[0];
			
		}
		else if ( 2 == vision ) 
		{
			num_keys = header.v2.lhdr.num_keys;
		}
		else
		{
			return READFDR_RC_68_UNKNOWN_FILE_FORMAT;
		}

		if (num_keys > 1)
		{
			*field = 'A';
		}
		else
		{
			*field = 'I';
		}
		
		wtrace("VISIONINFO","FT","VISION(%d) File=%s FileType=%c", vision, path, *field);
		return( READFDR_RC_0_SUCCESS );
	}

	if ( memcmp( code, "RT", 2 ) == 0 )				/* If looking for record type				*/
	{
		if (4 == vision)
		{
			if ( memcmp(&header.v4[V4_MAX_REC_OFF],&header.v4[V4_MIN_REC_OFF],2) == 0 ) 
				*field = 'F';
			else
			    	*field = 'V';

			if ( header.v4[V4_COMPRESS_OFF] ) 
				*field = 'C';
		}
		else if (3 == vision)
		{
			if ( memcmp(header.v3.max_rec,header.v3.min_rec,2) == 0 ) 
				*field = 'F';
			else					    
				*field = 'V';

			if ( header.v3.compress[0] ) 
				*field = 'C';
		}
		else if ( 2 == vision ) 
		{
			if ( header.v2.lhdr.var_recsize ) 
				*field = 'V';
			else			
				*field = 'F';

			if ( header.v2.lhdr.compressed ) 	
				*field = 'C';
		}
		else
		{
			return READFDR_RC_68_UNKNOWN_FILE_FORMAT;
		}
		
		wtrace("VISIONINFO","RT","VISION(%d) File=%s RecordType=%c", vision, path, *field);
		return( READFDR_RC_0_SUCCESS );
	}

	return( READFDR_RC_40_INVALID_INPUT_PARAM );
}

#ifdef unix
int unloadvision(const char *inname, const char *outname)		/* Unload the ACUCOBOL file to a tempfile		*/
{
	char	command[256];

	sprintf(command,"%s -unload '%s' '%s' ", acu_vutil_exe(), inname, outname );

	return 	run_unixcommand_silent(command);
}
#endif /* unix */


#ifdef WIN32
#include "win32spn.h"

int unloadvision(const char *inname, const char *outname)		/* Unload the ACUCOBOL file to a tempfile		*/
{
	char 	cmd[512];
	int 	rc;

	/*
	**	Spawn "vutil32.exe -unload <inname> <outname>"
	*/

	sprintf(cmd, "%s -unload %s %s", acu_vutil_exe(), inname, outname);
	ASSERT(strlen(cmd) < sizeof(cmd));
	
	rc = win32spawnlp(NULL, cmd, SPN_HIDDEN_CMD|SPN_WAIT_FOR_CHILD|SPN_NO_INHERIT);

	return( rc );
}
#endif /* WIN32 */

/*
**	History:
**	$Log: wfvision.c,v $
**	Revision 1.21.2.1.2.1  2002/10/09 21:03:05  gsl
**	Huge file support
**	
**	Revision 1.21.2.1  2002/08/19 15:31:05  gsl
**	4403a
**	
**	Revision 1.21  2001-11-12 17:56:17-05  gsl
**	Open in O_BINARY (WIN32)
**
**	Revision 1.20  2001-11-12 16:49:34-05  gsl
**	Add trace info
**
**	Revision 1.19  2001-11-12 16:27:20-05  gsl
**	VISION2 has 2 magic numbers (Big Endian & Little Endian)
**
**	Revision 1.18  2001-11-08 12:08:35-05  gsl
**	Add missing include
**
**	Revision 1.17  2001-11-08 12:06:50-05  gsl
**	add missing include
**
**	Revision 1.16  2001-10-30 15:27:24-05  gsl
**	Moved unloadvision() from wispsort.c
**	Removed visonversion() (unused)
**	Renamed acuvision() to visioninfo()
**	Changed logic to read whole header into union for v2,v3,v4 header
**
**	Revision 1.15  2001-10-29 10:46:02-05  gsl
**	Add IV index version
**
**	Revision 1.14  2001-10-24 15:32:03-04  gsl
**	Get num_keys for vision 2
**
**	Revision 1.13  2001-10-23 19:16:03-04  gsl
**	Rework with new return codes.
**	Add 'IX' function
**	Remove non-vision file support.
**	Add 'FT' support for 'A' alternate indexed files
**
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
