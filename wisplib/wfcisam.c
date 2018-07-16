static char copyright[]="Copyright (c) 1995 DevTech Migrations, All rights reserved.";
static char rcsid[]="$Id:$";
			/************************************************************************/
			/*									*/
			/*	        WISP - Wang Interchange Source Pre-processor		*/
			/*		       Copyright (c) 1988, 1989, 1990, 1991, 1992	*/
			/*	 An unpublished work of International Digital Scientific Inc.	*/
			/*			    All rights reserved.			*/
			/*									*/
			/************************************************************************/

/*
	cisaminfo:	Returns readfdr info for cisam files.
			Also supports Micro Focus 1.3 "FH ISAM" type files.  They look like cisam because they
			use 2 files "file" and "file.idx".
*/

/*
	cisam index header layout

	offset	length
	  0	  2	Magic number "FE53"
	  2	  4	(?) "02 02 04 04"
	  6	  2	Blocksize-1
	  8	  2	Number of index keys (0=relative file)
         10       1     (?) 00 or 07
	 11	  1     Duplicate key count width (2 or 4) MF uses 2.
	 12       1	(Unused)
	 13	  2	Data Record Length
	 15	  4	Address of key description (block number)
	 19	  6	(Unused)
	 25	  4	Address of data free list (block number)
	 29	  4	Address of index free list (block number)
	 33	  4	Number of records used on data file (before deletes)
	 37	  4	Number of index records used
	 41	  4	Transaction number
	 45	  4	Unique id
	 49	  4	audit trail info


	Micro Focus "FH ISAM" type files.  These files support data compression and variable length records.

	offset	length
	  0	  4	Length of file header? (First 4 bits are '0011' 3 for system record)
	                 "31 FE 00 00" (.idx with  512 byte node size)   
	                 "33 FE 00 00" (.idx with 1024 byte node size)   
	                 "3F FE 00 00" (.idx with 4096 byte node size)   
			 "30 7E 00 00" (.dat for record len less then 4095 bytes)
			 "30 00 00 7C" (.dat for record len >= then 4095 bytes)
         36	  2     Reserved (magic). Value 62 decimal; x"00 3E"
         39	  1	Organization 1=SEQ 2=IDX 3=REL
	 41	  1	Data compression 00=not compressed
         43	  1	Idx files only - IDXFORMAT version 3, 4, or 8
	 56	  2	Max record size (actually 54 for 4)
	 60	  2	Min record size (actually 58 for 4)

	200	  	Key info
	200	  2     number of keys = x-6/12   (first key is 18 bytes each alt key is 12 bytes)
	206	 12     Primary key
	218      12     Alternate key 1
	230      12     Alternate key 2 ...

			0  1  2  3  4  5  6  7  8  9  10 11
			00 0C 00 00 04 00 00 00 xx yy yy 00
			00 0C 00 00 06 00 00 dd xx yy yy 00
			00 0C 00 00 08 00 00 dd xx yy yy 00
			      a1    a2       b  c  d

			a1)	+2  4\	Maybe the starting block for key data +4:2 or +2:4
			a2)	+4  2/
			b)	+7  1	dupilcates allowed  00-no  80-yes
			c)	+8  1	key length
			d)	+9  2   key offset

	I did some investigation into the internals of a FH ISAM file.
	There doesn't seem to be a record count available.
	Starting a x400 is the primary keys data. The first 2 bytes of every block is a "used size" for that block, the high bit is
	not part of the size and is often set on - reason unknown.
	While writing it leaves half the block empty before starting next block.  When keys exceed 1 block then goes to
	a multi-level index scheme with the first block (x400) pointing to later blocks. Was not able to tell if there
	is maintained a index-levels counter or if there is an indicator that says the pointer is to another other index
	or data block.  Probably an indicator - maybe the last four bytes of block.  The 4th last byte of block
	seems to indicate the key x00-primary  x01- first alternate  x02- second alternate.
	An x80 in the 3rd last byte seem to mean that the block contain data pointers.  A x00 in the third last byte seems to mean
	a single level index scheme.

	Note:   If there are more index key definitions then fit in the x200 block then the key data probably start later then
		block x400. 
		It is possible that other block sizes are supported.
*/

#if defined(AIX) || defined(HPUX) || defined(SOLARIS) || defined(LINUX)
#define _LARGEFILE64_SOURCE
#define USE_FILE64
#endif

#include <stdio.h>
#include <errno.h>
#include <sys/types.h>
#include <fcntl.h>
#include <string.h>

#ifdef WIN32
#include <io.h>
#endif

#ifndef O_BINARY
#define O_BINARY 0
#endif
#ifndef O_TEXT
#define O_TEXT 0
#endif
#ifndef O_LARGEFILE
#define O_LARGEFILE 0
#endif

#include "idsistd.h"
#include "wisplib.h"
#include "osddefs.h"
#include "wfvision.h"
#include "werrlog.h"
#include "wmalloc.h"
#include "paths.h"

#define CISAM_ROOT_BLOCKSIZE_S 		6
#define CISAM_ROOT_NUM_KEYS_S 		8
#define CISAM_ROOT_RECORD_LENGHT_S 	13
#define CISAM_ROOT_DATA_FREE_LIST_L 	25
#define CISAM_ROOT_NUM_RECORDS_L 	33

#define CISAM_DFL_USED_S		0
#define CISAM_DFL_NEXT_L		2

#define FHISAM_STD_HEADER_SIZE		128
#define FHISAM_IDX_HEADER_SIZE		512

#define FHISAM_RESERVED_MAGIC_S		36
#define FHISAM_ORGANIZATION_B		39
#define FHISAM_DATA_COMPRESSION_B 	41
#define FHISAM_IDX_FORMAT_B 		43
#define FHISAM_MAX_RECORD_LENGHT_L 	54
#define FHISAM_MAX_RECORD_LENGHT_S 	56
#define FHISAM_MIN_RECORD_LENGHT_L 	58
#define FHISAM_MIN_RECORD_LENGHT_S 	60


#define FHISAM_IDX_NUM_KEYS_S		140

static	int4	chars_to_int( unsigned char *cp, int sz );			/* Convert string from cisam file into integer.	*/
										/* The following 2 macros are used to convert	*/
										/* multiple byte values in a cisam index header	*/
										/* into machine byte-order independent short	*/
										/* or 4-byte integers.  In a cisam index file,	*/
										/* the high-order byte always comes first	*/
										/* regardless of the machine architecture.	*/
#define	CTOS(p) ( (short) chars_to_int( (p), 2 ) )				/* 2 character bytes to short integer.		*/
#define CTOL(p) ( (int4)  chars_to_int( (p), 4 ) )				/* 4 character bytes to 4 byte integer.		*/

static int cisamRC(const char* path, int4* cnt);				/* Return the record count for a cisam file.	*/

int4 cisaminfo(				/* CISAM file system interface				*/
		const char *path,	/* File path						*/
		const char *code,	/* Function code					*/
		void	*raw_field)	/* Return info						*/
{
	char	path_idx[132];						/* Path with .idx extension				*/
	int	f;							/* File handle						*/
	int	i1;
	int4	*size;
	char	*field;
	int	is_idx = 0;						/* Has a .idx file */
	int	is_cisam = 0;						/* Is a "true" cisam file */
	int	is_mffh = 0;						/* Is a MF structured file */
	unsigned char	fheader[FHISAM_IDX_HEADER_SIZE];

	field = (char*) raw_field;
	size = (int4 *) field;

	strcpy(path_idx,path);
	strcat(path_idx,".idx");

	if (fexists(path_idx))
	{
		is_idx = 1;						/* Has .idx file					*/
		f = open( path_idx, O_RDONLY | O_BINARY | O_LARGEFILE );/* Open the .idx file					*/
	}
	else
	{
		is_idx = 0;
		f = open( path, O_RDONLY | O_BINARY | O_LARGEFILE );	/* Open the file					*/
	}

	if ( f == -1 )
	{
		return( READFDR_RC_44_IO_ERROR );
	}

	i1 = read( f, fheader, sizeof(fheader) );			/* read the header					*/
	close(f);

	if ( i1 == -1 )
	{
		return( READFDR_RC_44_IO_ERROR );
	}

	if ( i1 < FHISAM_STD_HEADER_SIZE)
	{
		/* 
		 * 	File is smaller then header
		 */
		return( READFDR_RC_24_NO_FILE_HEADER );
	}
	
	if ( is_idx && fheader[0] == 0xFE && fheader[1] == 0x53 ) 
	{ 
		is_cisam = 1;
	}
	else if ( 0x30 == (fheader[0] & 0xF0) &&			/* First 4 bits must be 3 (0011) */
		  0x00 == fheader[FHISAM_RESERVED_MAGIC_S+0] &&
		  0x3E == fheader[FHISAM_RESERVED_MAGIC_S+1]   )
	{
		is_mffh = 1;
	}
	else
	{
		/*
		 *	If not CISAM or MF FH then unknown 
		 */
		return( READFDR_RC_68_UNKNOWN_FILE_FORMAT );
	}

	if ( memcmp( code, "IX", 2 ) == 0 )				/* File Index Type */
	{
		if ( is_cisam )
		{
			*field = 'C';	/* CISAM */
			return( READFDR_RC_0_SUCCESS );
		}
		if ( is_mffh )
		{
			*field = 'M';	/* Micro Focus FHISAM */
			return( READFDR_RC_0_SUCCESS );
		}			
	}

	if ( memcmp( code, "IV", 2 ) == 0 )				/* File Index Type Version */
	{
		/*
		**	C0	CISAM Indexed
		**	R0	FHISAM Relative
		**	S0	FHISAM Sequential
		**	I3	FHISAM Indexed 3
		**	I4	FHISAM Indexed 4
		**	I8	FHISAM Indexed 8
		*/
		if ( is_cisam )
		{
			field[0] = 'C';
			field[1] = '0';
			return( READFDR_RC_0_SUCCESS );
		}
		if ( is_mffh )
		{
			switch( fheader[FHISAM_ORGANIZATION_B])
			{
			case 1: /* Sequential */
				field[0] = 'S';
				break;
			case 2: /* Indexed */
				field[0] = 'I';
				break;
			case 3: /* Relative */
				field[0] = 'R';
				break;
			default: /* Unknown */
				field[0] = 'U';		
				break;
			}

			switch(fheader[FHISAM_IDX_FORMAT_B])
			{
			case 3:
				field[1] = '3';
				break;	
			case 4:
				field[1] = '4';
				break;	
			case 8:
				field[1] = '8';
				break;	
			default:
				field[1] = '0';
				break;	
			}

			return( READFDR_RC_0_SUCCESS );
		}			
	}
	

	if ( memcmp( code, "RC", 2 ) == 0 )				/* If looking for record count				*/
	{
		if ( is_cisam ) 
		{
			return( cisamRC(path_idx,size) );
		}
		if ( is_mffh)
		{
			return( READFDR_RC_40_INVALID_INPUT_PARAM );	/* Record count is not available			*/
		}
	}

	if ( memcmp( code, "RS", 2 ) == 0 ||				/* If looking for record size/lenght			*/
	     memcmp( code, "RL", 2 ) == 0    )
	{
		if ( is_cisam )
		{
			*size = CTOS( &fheader[CISAM_ROOT_RECORD_LENGHT_S] );
			return( READFDR_RC_0_SUCCESS );
		}
		if ( is_mffh )
		{
			*size = CTOS( &fheader[FHISAM_MAX_RECORD_LENGHT_S] );
			return( READFDR_RC_0_SUCCESS );
		}
	}

	if ( memcmp( code, "FT", 2 ) == 0 )				/* If looking for file type				*/
	{
		if ( is_cisam )
		{
			int numkeys;
			numkeys = CTOS( &fheader[CISAM_ROOT_NUM_KEYS_S] );
			if (0 == numkeys) 
			{
				*field = 'R';	/* Relative */
			}
			else if ( 1 == numkeys )
			{
				*field = 'I';	/* Indexed */
			}
			else if ( numkeys > 1 )
			{
				*field = 'A';	/* Alternate-indexed */
			}
			else 		 
			{
				*field = 'U';	/* Unknown */
			}

			return( READFDR_RC_0_SUCCESS );
		}

		if ( is_mffh )
		{
			switch( fheader[FHISAM_ORGANIZATION_B])
			{
			case 1: /* Sequential */
				*field = 'C';		/* Consecutive */
				break;
			case 2: /* Indexed */
				if ( i1 == FHISAM_IDX_HEADER_SIZE &&
				     CTOS(&fheader[FHISAM_IDX_NUM_KEYS_S]) > 1) 
				{
					*field = 'A'; 	/* Alternate-indexed */
				}
				else
				{
					*field = 'I';	/* Indexed */
				}
				break;
			case 3: /* Relative */
				*field = 'R';		/* Relative */
				break;
			default: 
				*field = 'U';		/* Unknown */
				break;
			}

			return( READFDR_RC_0_SUCCESS );
		}
	}

	if ( memcmp( code, "RT", 2 ) == 0 )				/* If looking for record type				*/
	{
		if ( is_cisam ) 
		{
			*field = 'F';	/* Cisam only support fixed length files */
			return( READFDR_RC_0_SUCCESS );
		}

		if ( is_mffh )
		{
			if ( fheader[FHISAM_DATA_COMPRESSION_B] != '\0' )
				*field = 'C';
			else if ( CTOS( &fheader[FHISAM_MAX_RECORD_LENGHT_S] ) == 
				  CTOS( &fheader[FHISAM_MIN_RECORD_LENGHT_S] ) )
				*field = 'F';
			else
				*field = 'V';
			return( READFDR_RC_0_SUCCESS );
		}
	}

	return( READFDR_RC_40_INVALID_INPUT_PARAM );
}

static int cisamRC(const char* path, int4* cnt)			/* Return the record count for a cisam file. */
{
	FILE 	*fp;
	short	blocksize;
	unsigned char	buff[256];
	int	rc;
	int4	rcnt;									/* temp record count.			*/
	int4	nDataFreeList;
	int4	pos;									/* position to seek to			*/
	uint2	nUsedSize;

	fp = fopen(path,FOPEN_READ_BINARY);	
	if ( !fp )
	{
		return( READFDR_RC_44_IO_ERROR );
	}

	rc = fread(buff,64,1,fp);
	if ( rc != 1 ) 
	{
		fclose(fp);
		return( READFDR_RC_44_IO_ERROR );
	}

	blocksize = 1 + CTOS( &buff[CISAM_ROOT_BLOCKSIZE_S] );
	rcnt = CTOL( &buff[CISAM_ROOT_NUM_RECORDS_L] );					/* The rec cnt before any deletes.	*/
	nDataFreeList = CTOL( &buff[CISAM_ROOT_DATA_FREE_LIST_L] );

	while(nDataFreeList)
	{
		/*
		 *	Find the next "Free List" index block
		 */
		pos = (nDataFreeList-1)*blocksize;
		if (rc = fseek(fp,pos,0))
		{
			fclose(fp);
			return( READFDR_RC_44_IO_ERROR );
		}

		/*
		 *	Need to read the first 6 bytes of the block.
		 *	The first 2 bytes is the used size of the block. (zero the high nibble)
		 *	The next 4 bytes is the location of the next free list block
		 */
		rc = fread(buff,6,1,fp);
		if ( rc != 1 )
		{
			fclose(fp);
			return( READFDR_RC_44_IO_ERROR );
		}

		/*
		 *	Get used size of the block. 
		 *	Zero the high nibble of the first byte (not part of size).
		 */
		buff[CISAM_DFL_USED_S] &= 0x0F;
		nUsedSize = CTOS( &buff[CISAM_DFL_USED_S] );
		if( nUsedSize >= 6 )
		{
			/*
			 *	Calc how many deleted keys are in this block and
			 *	subtract them from the record count.
			 *	Each deleted record key takes 4 bytes.
			 *	The block header is 6 bytes.
			 */
			rcnt -= (CTOS( &buff[CISAM_DFL_USED_S] ) - 6) / 4;
		}

		/*
		 *	Get next Data Free List block number
		 */
		nDataFreeList = CTOL( &buff[CISAM_DFL_NEXT_L] );
	}

	*cnt = rcnt;
	fclose(fp);
	return( READFDR_RC_0_SUCCESS );
}

static	int4	chars_to_int( unsigned char *cp, int sz )		/* Convert string from cisam file into integer.		*/
/* cp	Pointer to first byte (high order) of string.	*/
/* sz	Numer of bytes to convert; 2 = short, 4 = int4.	*/
{
	int4	val;							/* Running total of characters processed so far.	*/
	int	i;							/* Index to each character to process in for loop.	*/

	val = 0;							/* Always start val at zero.				*/
	for( i = 0 ; i < sz ; ++i )					/* look at each character one at a time.		*/
	{
		val = ( val << 8 ) | ( 255 & (int4)cp[i] );		/* Shift previous val by 1 byte, then add next char.	*/
	}								/* "And" by 255L to insure getting just one byte.	*/

	return( val );							/* val holds the converted value of the characters.	*/
}

int unloadcisam(const char *inname, const char *outname, int4 recsize)
{
	FILE	*fpin, *fpout;
	unsigned char	*buff;
	int	rc;
	char	t_inname[132];

	strcpy(t_inname,inname);
	if (fexists(t_inname))
	{
		fpin = fopen(t_inname,FOPEN_READ_BINARY);
		if ( !fpin ) return(-1);
	}
	else
	{
		strcat(t_inname,".dat");
		if (fexists(t_inname))
		{
			fpin = fopen(t_inname,FOPEN_READ_BINARY);
			if ( !fpin ) return(-1);
		}
		else return(-1);	 
	}

	fpout = fopen(outname,FOPEN_WRITE_BINARY);
	if ( !fpout ) 
	{
		fclose(fpin);
		return(-1);
	}

	buff = (unsigned char *)wmalloc((size_t)recsize+2);
		
	for(;;)
	{
		rc = fread(buff,(size_t)recsize+1,1,fpin);
		if ( rc != 1 )
		{
			if (feof(fpin)) break;
			fclose(fpin);
			fclose(fpout);
			free(buff);
			return(-1);
		}

		if ( buff[recsize] == 0x0A )
		{
			rc = fwrite(buff, (size_t)recsize, 1, fpout);
			if ( rc != 1 )
			{
				fclose(fpin);
				fclose(fpout);
				free(buff);
				return(-1);
			}
		}
		else if ( buff[recsize] )
		{
			fclose(fpin);
			fclose(fpout);
			free(buff);
			return(-1);
		}
	}

	fclose(fpin);
	fclose(fpout);
	free(buff);
	return(0);
}

#ifdef WIN32
int unloadfhisam(const char *inname, const char *outname, int4 recsize)
{
	werrlog(102, "(unloadfhism) Not Implemented",0,0,0,0,0,0,0);
	return 1;
}
#endif /* WIN32 */

#ifdef unix

static int unloadfhisam_fhconvert(const char *inname, const char *outname, int4 recsize)
{
	char	parms[512];
	char	cmd[512];
	char	buff[80];
	char	*filetype;
	char	indexversion[3];
	char	recordtype;

	if ( 0 != cisaminfo(inname, "IV", indexversion))
	{
		wtrace("UNLOAD","FAILED", "Unable to get Index Version for file=[%s]", inname);
		return 1;
	}
	indexversion[2] = '\0';

	if ( 0 != cisaminfo(inname, "RT", &recordtype))
	{
		wtrace("UNLOAD","FAILED", "Unable to get Record Type for file=[%s]", inname);
		return 1;
	}
	if ('C' == recordtype)
	{
		wtrace("UNLOAD","FAILED", "fhconvert is unable to unload COMPRESSED file=[%s]", inname);
		return 1;
	}

	if (0==strcmp(indexversion,"C0"))
	{
		filetype = "I0";
	}
	else if (0==strcmp(indexversion,"I3"))
	{
		filetype = "I3";
	}
	else if (0==strcmp(indexversion,"I4"))
	{
		filetype = "I4";
	}
	else if (0==strcmp(indexversion,"I8"))
	{
		wtrace("UNLOAD","FAILED", "fhconvert does not work with IDXFORMAT 8 files (use rebuild) file=[%s]", inname);
		return 1;
	}
	else if ('R' == indexversion[0])
	{
		filetype = "R0";
	}
	else if ('S' == indexversion[0])
	{
		filetype = "S0";
	}
	else
	{
		wtrace("UNLOAD","FAILED", "Unrecognized index version=[%s] file=[%s]", indexversion, inname);
		return 1;
	}

	if (fexists(inname))
	{
		/*
		**	No .dat extension
		*/
		sprintf(parms,"IN %s\nIE\nIT %s\nON %s\nOE\nOT S0\nOF %d\n",inname, filetype, outname, recsize);
	}
	else
	{
		sprintf(parms,"IN %s\nIT %s\nON %s\nOT S0\nOF %d\n",inname, filetype, outname, recsize);
	}

	sprintf(cmd,"echo '%s' | fhconvert -e -c - ",parms);

	unlink(outname);		/* Delete old output file */

	run_unixcommand_silent(cmd);

	/* Delete temp .con files that fhconvert creates */
	strcpy(buff,inname);
	strcat(buff,".con");
	unlink(buff);

	strcpy(buff,outname);
	strcat(buff,".con");
	unlink(buff);

	if (fexists(outname))
	{
		return 0;
	}
	else
	{
		wtrace("UNLOAD","FAILED", "fhconvert failed to create outfile=[%s]", outname);
		return 1;
	}
}

static int unloadfhisam_rebuild(const char *inname, const char *outname, int4 recsize)
{
	char	cmd[512];
	char	*intype;
	char	indexversion[3];

	if ( 0 != cisaminfo(inname, "IV", &indexversion))
	{
		wtrace("UNLOAD","FAILED", "Unable to get Index Version for file=[%s]", inname);
		return 1;
	}
	indexversion[2] = '\0';

	if (0==strcmp(indexversion,"C0"))
	{
		intype = "ind";
	}
	else if ('I' == indexversion[0])
	{
		intype = "ind";
	}
	else if ('R' == indexversion[0])
	{
		intype = "rel";
	}
	else if ('S' == indexversion[0])
	{
		intype = "seq";
	}
	else
	{
		wtrace("UNLOAD","FAILED", "Unrecognized index version=[%s] file=[%s]", indexversion, inname);
		return 1;
	}

	/*
	**	rebuild {infile},{outfile} -o:{ind},seq -c:d0 -r:f{recsize}
	*/
	sprintf(cmd,"rebuild '%s,%s' -o:%s,seq -c:d0 -r:f%d ",
		inname, outname, intype, recsize);

	unlink(outname);		/* Delete old output file */

	run_unixcommand_silent(cmd);

	if (fexists(outname))
	{
		return 0;
	}
	else
	{
		wtrace("UNLOAD","FAILED", "rebuild failed to create outfile=[%s]", outname);
		return 1;
	}
}

int unloadfhisam(const char *inname, const char *outname, int4 recsize)
{
#define UNLOAD_NOTSET		-1
#define UNLOAD_MISSING		-2
#define UNLOAD_FHCONVERT	0
#define UNLOAD_REBUILD		1
	static int unload_method = UNLOAD_NOTSET;

	if (UNLOAD_NOTSET == unload_method) /* First time */
	{
		char buff[256];
	  	if (!get_wisp_option("UNLOADREBUILD") &&
		    0 == whichenvpath("fhconvert", buff))
		{
			unload_method = UNLOAD_FHCONVERT;
		}
		else if (0 == whichenvpath("rebuild", buff))
		{
			unload_method = UNLOAD_REBUILD;
		}
		else
		{
			unload_method = UNLOAD_MISSING;
		}
	}

	if (UNLOAD_REBUILD==unload_method)
	{
		return unloadfhisam_rebuild(inname, outname, recsize); 
	}
	else if (UNLOAD_FHCONVERT==unload_method)
	{
		return unloadfhisam_fhconvert(inname, outname, recsize);
	}
	else
	{
		werrlog(104,"%%UNLOAD-E-MISSING FHISAM Unload method missing (fhconvert or rebuild)",0,0,0,0,0,0,0);
		return 1;
	}
}

#endif /* unix */

/*
**	History:
**	$Log: wfcisam.c,v $
**	Revision 1.21.2.1.2.2  2002/11/19 16:24:04  gsl
**	Define O_LARGEFILE for ALPHA and SCO
**	
**	Revision 1.21.2.1.2.1  2002/10/09 21:03:04  gsl
**	Huge file support
**	
**	Revision 1.21.2.1  2002/08/19 15:31:04  gsl
**	4403a
**	
**	Revision 1.21  2001-11-12 17:42:19-05  gsl
**	OPEN file in O_BINARY mode
**	(affects WIN32 only)
**
**	Revision 1.20  2001-11-08 12:05:51-05  gsl
**	Add missing include
**
**	Revision 1.19  2001-10-29 18:03:41-05  gsl
**	fhconvert checks for compressed files and fails.
**
**	Revision 1.18  2001-10-26 16:15:57-04  gsl
**	Add cisaminfo("IV") to get the index version.
**	Moved unloadcisam() and unloadfhisam() from wispsort.c
**	Write unloadfhisam_rebuild()
**
**	Revision 1.17  2001-10-25 15:14:18-04  gsl
**	Fixed CISAM RC on HP 11
**	The used size of a block was incorrectly using the high nibble of the
**	first byte.
**	Document the CISAM structure,
**
**	Revision 1.16  2001-10-23 19:20:28-04  gsl
**	If not header return RC=24
**
**	Revision 1.15  2001-10-22 17:02:14-04  gsl
**	Updated for MF FHISAM files.
**	Does not support non-isam files
**
**	Revision 1.14  2001-10-19 16:42:56-04  gsl
**	Removed unneeded ifdefs
**	Replace literals with defines
**
**	Revision 1.13  1997-07-29 14:56:10-04  gsl
**	Add the new ID3 and ID4 magic numbers for micro focus
**
**	Revision 1.12  1997-03-12 13:21:21-05  gsl
**	Changed to use WIN32 define
**
**	Revision 1.11  1996-09-10 11:49:55-04  gsl
**	Fix uninitailized variable "field"
**
**	Revision 1.10  1996-08-19 15:33:13-07  gsl
**	drcs update
**
**
**
*/
